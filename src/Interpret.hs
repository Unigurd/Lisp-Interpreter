module Interpret where

import LispVal (LispError, LispVal(..), eval, Table(Table))
import Parse (build)
import Builtins (builtins)

interpret :: String -> LispError
interpret str = (build str) >>= eval (Table builtins)
i exp = interpret ("(" ++ exp ++ ")") -- shorthand
