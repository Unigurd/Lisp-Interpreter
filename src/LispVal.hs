module LispVal where

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

infixr 5 :.

newtype Table = Table LispVal
  deriving (Eq)

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


wrongArgs str = Left ("Wrong arguments given to " ++ str)

lookUp :: Table -> LispVal -> LispError
lookUp (Table ((Atom x:.y):.xys)) (Atom key) = 
  case x == key of
    True  -> Right y
    False -> lookUp (Table xys) (Atom key)
-- If an element in the table is identified by something other than an Atom, we just skip right past
lookUp (Table ((_:._):.xys)) (Atom key) = lookUp (Table xys) (Atom key)
lookUp (Table Nil) (String key) = Left $ "Unknown symbol" ++ (show key)
lookUp _ _ = wrongArgs "lookup"

define :: Table -> String -> LispVal -> Either String Table
define (Table table) name value =
  Right (Table ((Atom name:.value):.table))

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

