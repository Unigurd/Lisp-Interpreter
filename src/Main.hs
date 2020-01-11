{-# LANGUAGE RankNTypes #-}
import Text.Read hiding (String, Symbol)
import Control.Monad

data Token = 
    Open 
  | Closed 
  | Word LispVal
  deriving (Show)

data LispVal =
    Atom String
  | LispVal :. LispVal
  | Nil
  | Int Int
  | T
  | String String
  | PrimFun ((Table -> LispVal -> LispError), String)
  | Lambda Table
  --deriving (Eq)

instance Eq LispVal where
  (==) (Atom str1) (Atom str2) = str1 == str2
  (==) (car1 :. cdr1) (car2 :. cdr2) = car1 == car2 && cdr1 == cdr2
  (==) Nil Nil = True
  (==) (Int int1) (Int int2) = int1 == int2
  (==) T T = True
  (==) (String str1) (String str2) = str1 == str2
  (==) (PrimFun (_, funName1)) (PrimFun (_, funName2)) = funName1 == funName2
  (==) (Lambda table1) (Lambda table2) = table1 == table2
  (==) _ _ = False

instance Show LispVal where
  show lispVal =
    case lispVal of
      (car :. cdr) -> "(" ++ (showHelp (car :. cdr)) ++ ")"
      other -> showHelp other
    where
      showHelp (Atom string) =
        string
      showHelp ((caar :. cadr) :. cdr) =
        "(" ++ (showHelp (caar :. cadr)) ++ ") " ++ (showHelp cdr)
      showHelp (car :. (cdar :. cddr)) =
        (showHelp car) ++ " " ++ (showHelp (cdar :. cddr))
      showHelp (car :. Nil) = 
        showHelp car
      showHelp (car :. cdr) = 
        (showHelp car) ++ " . " ++ (showHelp cdr)
      showHelp Nil = 
        "()"
      showHelp (Int int) = show int
      showHelp T = "t"
      showHelp (String string) = show string
      showHelp (PrimFun (_, name)) = name
      showHelp (Lambda _) = "lambda"

type LispError = Either String LispVal

data Progress = Finished | Unfinished 
newtype Table = Table [(String, LispVal)] 
  deriving (Eq)

-- foldl for LispVals
lFold :: (a -> LispVal -> a) -> a -> LispVal -> a
lFold fun acc (car :. cdr) = lFold fun (fun acc car) cdr 
lFold fun acc other = fun acc other

-- Folding of two LispVals simultaneously
-- Should maybe be lFold . lZip?
lFold2 :: (a -> LispVal -> LispVal -> a) -> a -> LispVal -> LispVal -> Either String a
lFold2 fun acc (car1 :. cdr1) (car2 :. cdr2) = lFold2 fun (fun acc car1 car2) cdr1 cdr2
lFold2 _ acc Nil Nil = Right acc
lFold2 _ _ _ _ = Left "Something went wrong in internal function \"lFold2\""

-- foldlM for lispVals
lFoldM :: Monad m => (a -> LispVal -> m a) -> a -> LispVal -> m a
lFoldM fun acc (car :. cdr) = do
  newAcc <- fun acc car
  lFoldM fun newAcc cdr
lFoldM fun acc other = fun acc other

--lFoldM2 :: Monad m => (a -> LispVal -> LispVal -> m a) -> a -> LispVal -> LispVal -> Either String (m a)
--lFoldM2 fun acc (car1 :. cdr1) (car2 :. cdr2) = do
--  newAcc <- fun acc car1 car2
--  lFoldM2 fun newAcc cdr1 cdr2
--lFoldM2 _ acc Nil Nil = Right (return acc)
--lFoldM2 _ _ _ _ = Left "Something went wrong in internal function \"lFoldM2\""

-- map for LispVals
-- Should perhaps be implemented in terms of lFold?
lMap :: (LispVal -> LispVal) -> LispVal -> LispVal
lMap fun (car :. cdr) = (fun car) :. (lMap fun cdr)
lMap fun other = fun other

-- monadic mapping for lispVals
lMapM :: Monad m => (LispVal -> m LispVal) -> LispVal -> m LispVal
lMapM fun (car :. cdr) = do
  carResult <- fun car
  cdrResult <- lMapM fun cdr
  return (carResult :. cdrResult)
lMapM fun other = fun other

main :: IO ()
main = do 
  putStr "=> "
  line <- getLine
  case line of
    ":q" -> return ()
    _    -> do
      putStrLn $ case interpret line of
        Right result -> show result
        Left error   -> "Error: " ++ error
      main

removeLeadingWhiteSpace :: String -> String
removeLeadingWhiteSpace (' ':str) =
  removeLeadingWhiteSpace str
removeLeadingWhiteSpace ('\n':str) =
  removeLeadingWhiteSpace str
removeLeadingWhiteSpace str =
  str

-- Like words, but treats ( and ) as separate words regardless of whitespace
lWords :: String -> [String]
lWords str =
  case lWord str of 
    (word, [])   -> [word]
    (word, rest) -> word:(lWords rest)
  where
    lWord :: String -> (String, String)
    lWord = lWordHelp . removeLeadingWhiteSpace

    lWordHelp :: String -> (String, String)
    lWordHelp ('(':str) =
      ("(", str)
    lWordHelp (')':str) =
     (")", str)
    lWordHelp (' ':str) =
      ("", str)
    lWordHelp ('\n':str) =
      ("", str)
    lWordHelp (letter:'(':str) =
      ([letter],'(':str)
    lWordHelp (letter:')':str) =
      ([letter],')':str)
    lWordHelp (letter:str) =
      let (word, rest) = lWordHelp str in
      (letter:word, rest)
    lWordHelp [] =
      ("","")


toToken :: String -> Token
toToken "(" = Open
toToken ")" = Closed
toToken str = Word . toAtom $ str

-- lexx because lex is in prelude
lexx :: String -> [Token]
lexx = map toToken . lWords

-- Returns Left when successfully read, Right with whatever was attempted read upon failure
readEitherFlip :: Read a => (a -> LispVal) -> String -> Either LispVal String
readEitherFlip wrap str =
  case readMaybe str of
    Nothing -> Right str
    Just a  -> Left (wrap a)

toAtom :: String -> LispVal
toAtom str = 
  let tried = (return str) >>= 
              (readEitherFlip Int) >>=
              (readEitherFlip String) >>=
              (\x -> if x == "lambda" then Left (Lambda (Table [])) else Right x) >>=
              (\x -> if x == "t" then Left T else Right x) in
  case tried of
    Left result -> result
    Right str  -> Atom str

parse :: [Token] -> LispError
parse tokens =
  case tokens of
    (Open:tokens) -> do
      (lispVal, progress, _) <- parseHelp $ (Open:tokens)
      case progress of
        Unfinished -> Left "Missing opening bracket"
        Finished   -> Right lispVal
    [Word (Atom "")] -> Left ""
    _  -> Left "Missing opening bracket"

  where
    parseHelp :: [Token] -> Either String (LispVal, Progress, [Token])
    parseHelp (Open:tokens) = do
      result1 <- parseHelp tokens
      case result1 of
        (innerList, _, Closed:[]) -> return (innerList, Finished, [])
        (innerList, _, Closed:tmpTokens) -> do
          result2 <- parseHelp tmpTokens
          case result2 of
            (head :. tail, _, tokens) -> Right (innerList :. (head :. tail), Unfinished, tokens)
            (Nil, _, tokens) -> Right (innerList :. Nil, Unfinished, tokens)
            _   -> Left "Something went wrong"
        _ -> Left "Unmatched opening bracket"
    
    parseHelp (Closed:tokens) =
      Right (Nil, Unfinished, Closed:tokens)
    
    parseHelp (Word word:tokens) = do
      (lispVal, _, rest) <- parseHelp tokens
      Right (word :. lispVal, Unfinished, rest)
    
    parseHelp [] =
      Left "Unexpected end of expression"

build = parse . lexx

wrongArgs str = Left ("Wrong arguments given to " ++ str)

lookUp :: Table -> LispVal -> LispError
lookUp (Table ((x,y):xys)) (Atom key) = 
  case x == key of
    True  -> Right y
    False -> lookUp (Table xys) (Atom key)
lookUp (Table []) (String key) = Left $ "Unknown symbol" ++ (show key)
lookUp _ _ = wrongArgs "lookup"

define :: Table -> String -> LispVal -> Either String Table
define (Table table) name value =
  Right (Table ((name, value):table))

tmpBind :: Table -> LispVal -> LispVal -> Either String Table
tmpBind table Nil Nil = return table -- equally many parameters and arguments
tmpBind table (Atom name :. Nil) rest@(_ :. (_ :. _)) = do 
  -- bind the list of the remaining arguments to the last parameter
  newTable <- define table name rest
  return newTable
tmpBind table (Atom name :. cdr1) (value :. cdr2) = do
  -- bind an argument value to a param name
  newTable <- define table name value
  tmpBind newTable cdr1 cdr2
tmpBind _ (_ :. _) Nil =
  -- Error if too few arguments are supplied. Might implement currying sometime
  Left "Not enough arguments supplied"

apply :: Table -> LispVal -> LispError
apply table (car :. cdr) = do
  head <- eval table car
  case head of
    (PrimFun (fun, _)) -> fun table cdr
    lambda@(Lambda _)  -> return lambda
    (((Lambda closure) :. (params :. body)) :. args) -> do
      newTable <- tmpBind closure params args
      lMapM (eval newTable) body
    _ -> wrongArgs "apply"
apply _ args = wrongArgs "apply"

eval :: Table -> LispVal -> LispError
eval table atom@(Atom _) = lookUp table atom
eval table list@(_ :. _) = apply table list
eval closure (Lambda _)  = Right (Lambda closure)
eval _ other = return other

quote _ (elm :. Nil) = Right elm
quote _ _ = Left "Function quote not applied to single argument"

car table rawArgs = do
  args <- lMapM (eval table) rawArgs 
  case args of
    (Nil :. Nil)            -> Right Nil
    ((head :. rest) :. Nil) -> Right head
    _ -> Left "Function car not applied to a single list"

cdr table rawArgs = do
  args <- lMapM (eval table) rawArgs  
  case args of
   (Nil :. Nil)            -> Right Nil 
   ((head :. rest) :. Nil) -> Right rest
   _ -> Left "Function cdr not applied to a single list"

atom table rawArgs = do
  args <- lMapM (eval table) rawArgs
  case args of
    ((_ :. _) :. Nil) -> Right Nil
    (_ :. Nil) -> Right T

cons table rawArgs = do
  args <- lMapM (eval table) rawArgs
  case args of
    (car :. (cdr :. Nil)) -> Right (car :. cdr)
    _ -> wrongArgs "cons"

lIf table (pred :. (trueExp :. (falseExp :. Nil))) = do
  result <- eval table pred
  case result of
    Nil -> eval table falseExp
    _   -> eval table trueExp
lIf _ _ = wrongArgs "if"


plus table rawArgs = do
  args <- lMapM (eval table) rawArgs
  lFoldM lPlus (Int 0) args
  where
    lPlus acc elm =
      case (acc, elm) of
        (Int a, Int b) -> Right (Int (a + b))
        (acc, Nil)     -> Right acc
        _              -> Left "Wrong type given to +"  

throw table (rawArg :. Nil) = do
  arg <- eval table rawArg
  Left $ show arg
raise _ _ =
  wrongArgs "throw"

catch table (try :. (except :. Nil)) =
  case eval table try of
    Left err -> eval table (except :. ((String err) :. Nil))
    Right result -> Right result
catch _ _ =
  wrongArgs "catch"

append table rawArgs = do
  args <- lMapM (eval table) rawArgs
  lFoldM lAppend (String "") args
  where
    lAppend acc elm =
      case (acc, elm) of
        (String a, String b) -> Right (String (a ++ b))
        (acc, Nil)           -> Right (acc) 
        (_, _)               -> Left "Wrong type given to append"  

temp table ((String str) :. Nil) =
  Right (String ("temp fun to test catch by appending: " ++ str))
temp _ _ = wrongArgs "temp"

builtins =
  [("car", PrimFun (car, "car")),
   ("cdr", PrimFun (cdr, "cdr")),
   ("apply", PrimFun (apply, "apply")),
   ("eval", PrimFun (eval, "eval")),
   ("quote", PrimFun (quote, "quote")),
   ("atom", PrimFun (atom, "atom")),
   ("cons", PrimFun (cons, "cons")),
   ("if", PrimFun (lIf, "if")),
   ("+", PrimFun (plus, "+")),
   ("throw", PrimFun (throw, "throw")),
   ("catch", PrimFun (catch, "catch")),
   ("append", PrimFun (append, "append")),
   ("temp", PrimFun (temp, "temp"))
  ]

interpret :: String -> LispError
interpret str = (build str) >>= eval (Table builtins)
i exp = interpret ("(" ++ exp ++ ")") -- shorthand

interpretTests =
  [("", Left ""),
   ("1", Left "Missing opening bracket"),
   ("1 2", Left "Missing opening bracket"),
   (")", Left "Missing opening bracket"),
   ("1)", Left "Missing opening bracket"),
   ("(", Left "Unexpected end of expression"),
   ("(1 2 3)", Left "Int 1 is not a function"),
   ("(quote 1)", Right (Int 1)),
   ("(quote 2)", Right (Int 2)),
   ("(quote (1))", Right (Int 1 :. Nil)),
   ("(quote (1 2))", Right (Int 1 :. (Int 2 :. Nil ))),
   ("(quote ((1 2) 3))", Right ((Int 1 :. (Int 2 :. Nil )) :. (Int 3 :. Nil))),
   ("(quote)", Left "Function quote not applied to single argument"),
   ("(quote 1 2)", Left "Function quote not applied to single argument"),
   ("(quote (1 2) 3)", Left "Function quote not applied to single argument"),
   ("(quote 1 2 3)", Left "Function quote not applied to single argument"),
   ("(car (quote (1)))", Right (Int 1)),
   ("(car (quote (1 2)))", Right (Int 1)),
   ("(car 1)", Left "Function car not applied to a single list"),
   ("(car)", Left "Function car not applied to a single list"),
   ("(car (quote (1 2)) (quote (3 4)))", Left "Function car not applied to a single list")
  ]


test f t = map (\x -> f (fst x) == snd x) t

testInterpret = test interpret interpretTests

{- 
lambda
label
eq

atom
car cdr
cons
quote
cond
apply

int?
float?
+
-
*
/
%

not
and
or

-}
