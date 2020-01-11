module Builtins where
import LispVal 
  (LispError, LispVal(..), Table(Table),
   eval, apply, lMapM, lFoldM, wrongArgs)

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
  ((Atom "car"    :. PrimFun (car,    "car")) :.
   (Atom "cdr"    :. PrimFun (cdr,    "cdr")) :.
   (Atom "apply"  :. PrimFun (apply,  "apply")) :.
   (Atom "eval"   :. PrimFun (eval,   "eval")) :.
   (Atom "quote"  :. PrimFun (quote,  "quote")) :.
   (Atom "atom"   :. PrimFun (atom,   "atom")) :.
   (Atom "cons"   :. PrimFun (cons,   "cons")) :.
   (Atom "if"     :. PrimFun (lIf,    "if")) :.
   (Atom "+"      :. PrimFun (plus,   "+")) :.
   (Atom "throw"  :. PrimFun (throw,  "throw")) :.
   (Atom "catch"  :. PrimFun (catch,  "catch")) :.
   (Atom "append" :. PrimFun (append, "append")) :.
   (Atom "temp"   :. PrimFun (temp,   "temp")) :.
   Nil)
  

--builtins =
--  [("car", PrimFun (car, "car")),
--   ("cdr", PrimFun (cdr, "cdr")),
--   ("apply", PrimFun (apply, "apply")),
--   ("eval", PrimFun (eval, "eval")),
--   ("quote", PrimFun (quote, "quote")),
--   ("atom", PrimFun (atom, "atom")),
--   ("cons", PrimFun (cons, "cons")),
--   ("if", PrimFun (lIf, "if")),
--   ("+", PrimFun (plus, "+")),
--   ("throw", PrimFun (throw, "throw")),
--   ("catch", PrimFun (catch, "catch")),
--   ("append", PrimFun (append, "append")),
--   ("temp", PrimFun (temp, "temp"))
--  ]

