module Parse where

import Text.Read hiding (String, Symbol)
import LispVal

data Token = 
    Open 
  | Closed 
  | Word LispVal
  deriving (Show)

data Progress = Finished | Unfinished 

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

