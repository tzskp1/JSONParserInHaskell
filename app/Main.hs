import Text.Parsec
import Data.Functor.Identity
import Data.Char
import GHC.IO.Encoding

data JStr = JStr [Char] deriving Show

data JValue =
    JStrAsVal JStr
  | JNumber (Int,Int,Int)        -- int,frac,exp
  | JObject [(JStr,JValue)]
  | JArray [JValue]
  | JTrue
  | JFalse
  | JNull
  deriving Show

intOfStr [] = error "intOfStr"
intOfStr xs = foldl (\a b -> 10 * a + b) 0 $ map digitToInt xs
-- なぜか型注釈がないとコンパイルエラーになる
fracPart :: ParsecT [Char] u Identity Int
fracPart = ((char '.') *> (intOfStr <$> ((:) <$> digit <*> many digit)))
           <|> (return 0)
expPart :: ParsecT [Char] u Identity Int
expPart = ((char 'e' <|> char 'E')
           *> (char '+' *> (intOfStr <$> ((:) <$> digit <*> many digit))
           <|> (char '-' *> (((* (-1)) . intOfStr) <$> ((:) <$> digit <*> many digit)))
           <|> (intOfStr <$> ((:) <$> digit <*> many digit))))
          <|> (return 0)
intPart :: ParsecT [Char] u Identity Int
intPart = (char '-' *> (((* (-1)) . intOfStr)
                        <$> ((:) <$> oneOf "123456789" <*> many digit)))
          <|> (intOfStr <$> ((:) <$> oneOf "123456789" <*> many digit))
numParser =
  let numOf [x,y,z] = JNumber (x,y,z)
  in numOf <$> sequence [intPart,fracPart,expPart]

parenParser :: Char -> Char -> [Char] -> ParsecT [Char] u Identity v -> ParsecT [Char] u Identity [v]
parenParser st_char ed_char sep_lst psr =
  let parenS = char st_char
      parenE = char ed_char *> return []
      loop = (:) <$> psr <*> (parenE <|> ((sequence . (map char)) sep_lst *> loop))
  in (try (parenS *> parenE)) <|> (parenS *> loop)

hexPart = let hex = oneOf "0123456789ABCDEFabcdef"
              charOfHex xs = chr $ foldl (\a b -> 16 * a + b) 0 $ map digitToInt xs
          in char 'u' *> (charOfHex <$> sequence [hex,hex,hex,hex])
escChar = char '\\'
          *> (hexPart
              <|> (char '"' *> return '"')
              <|> (char '\\' *> return '\\')
              <|> (char '/' *> return '/')
              <|> (char 'b' *> return '\BS')
              <|> (char 'f' *> return '\FF')
              <|> (char 'n' *> return '\n')
              <|> (char 'r' *> return '\r')
              <|> (char 't' *> return '\t'))
strParser = JStr <$> (parenParser '"' '"' [] (escChar <|> anyChar))

arrParser = JArray <$> (parenParser '[' ']' "," valParser)

objParser = JObject <$> (parenParser '{' '}' ","
                          $ (\x y -> (x,y))
                          <$> (spaces *> strParser <* spaces)
                          <*> (char ':' *> valParser))

boolParser = (string "true" *> return JTrue) <|> (string "false" *> return JFalse)

nullParser = (string "null" *> return JNull)

valParser = spaces
            *> ((JStrAsVal <$> strParser) <|> numParser
                <|> objParser <|> arrParser
                <|> boolParser <|> nullParser)
            <* spaces

main = setLocaleEncoding utf8
       >> (readFile "./test.json" >>= \js -> print $ parse valParser "" js)
