{-# LANGUAGE RankNTypes #-}
import Text.Read hiding (String, Symbol)
import Control.Monad
import LispVal
import Parse (build)

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
