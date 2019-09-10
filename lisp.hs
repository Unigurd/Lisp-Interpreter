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
--  | Function (Table -> LispVal -> LispVal)
  | Int Int
  | T
  | String String
  deriving (Eq)

instance Show LispVal where
  show (car :. cdr) = "(" ++ (showHelp (car :. cdr)) ++ ")"
  show other = showHelp other

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
--showHelp (Function _) = "lambda"
showHelp (Int int) = show int
showHelp T = "t"
showHelp (String string) = show string

type LispError = Either String LispVal

data Progress = Finished | Unfinished 
newtype Table = Table [(String, Table -> LispVal -> LispError)] 

lFold :: (a -> LispVal -> a) -> a -> LispVal -> a
lFold fun acc (car :. cdr) = lFold fun (fun acc car) cdr 
lFold fun acc other = fun acc other

lFoldM :: Monad m => (a -> LispVal -> m a) -> a -> LispVal -> m a
lFoldM fun acc (car :. cdr) = do
  newAcc <- fun acc car
  lFoldM fun newAcc cdr
lFoldM fun acc other = fun acc other

lMap :: (LispVal -> LispVal) -> LispVal -> LispVal
lMap fun (car :. cdr) = (fun car) :. (lMap fun cdr)
lMap fun other = fun other

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

lWord :: String -> (String, String)
lWord = lWordHelp . removeLeadingWhiteSpace

lWords :: String -> [String]
lWords str =
  case lWord str of 
    (word, [])   -> [word]
    (word, rest) -> word:(lWords rest)

toToken :: String -> Token
toToken "(" = Open
toToken ")" = Closed
toToken str = Word . toAtom $ str

lexx :: String -> [Token]
lexx = map toToken . lWords

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
              (\x -> if x == "t" then Left T else Right x) in
  case tried of
    Left result -> result
    Right str  -> Atom str

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

parse :: [Token] -> LispError
parse (Open:tokens) = do
  (lispVal, progress, _) <- parseHelp $ (Open:tokens)
  case progress of
    Unfinished -> Left "Missing opening bracket"
    Finished   -> Right lispVal
parse [Word (Atom "")] = Left ""
parse _ = Left "Missing opening bracket"

build = parse . lexx

lookUp :: Table -> String -> Either String (Table -> LispVal -> LispError) 
lookUp (Table ((x,y):xys)) key = 
  case x == key of
    True  -> Right y
    False -> lookUp (Table xys) key
lookUp (Table []) key = Left $ "Unknown symbol" ++ (show key)

apply :: Table -> LispVal -> LispError
apply table ((Atom funName) :. args)  = do
  fun <- lookUp table funName
  fun table args
apply _ _ = Left "Wrong arguments given to apply"

quote _ (elm :. Nil) = Right elm
quote _ _ = Left "Function quote not applied to single argument"

eval :: Table -> LispVal -> LispError
eval table list@((Atom fun) :. args) = apply table list
eval _ (notFun :. _) = Left ((show notFun) ++ " is not a function")
eval _ other = return other

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
    _ -> Left "Wrong number of arguments given to cons"

lIf table (pred :. (trueExp :. (falseExp :. Nil))) = do
  result <- eval table pred
  case result of
    Nil -> eval table falseExp
    _   -> eval table trueExp
lIf _ _ = Left "Wrong number of arguments given to if"


plus table rawArgs = do
  args <- lMapM (eval table) rawArgs
  lFoldM lPlus (Int 0) args
  where
    lPlus acc elm =
      case (acc, elm) of
        (Int a, Int b) -> Right (Int (a + b))
        (acc, Nil)     -> Right acc
        _              -> Left "Wrong type given to +"  


builtins =
  [("car", car),
   ("cdr", cdr),
   ("apply", apply),
   ("quote", quote),
   ("atom", atom),
   ("cons", cons),
   ("if", lIf),
   ("+", plus)
  ]

interpret :: String -> LispError
interpret str = (build str) >>= eval (Table builtins)

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
