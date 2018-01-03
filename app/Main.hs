import Text.Parsec
import Data.Functor.Identity
import Data.Char
import GHC.IO.Encoding

data JStr = JStr [Char] deriving Show

data JValue =
    JStrOfVal JStr
  | JNumber Double
  | JObject [(JStr,JValue)]
  | JArray [JValue]
  | JTrue
  | JFalse
  | JNull
  deriving Show

intOfStr xs = foldl (\a b -> 10 * a + b) 0.0 $ map (fromIntegral . digitToInt) xs
fracPart =
  let frcOfStr xs = foldr (\a b -> (b / 10.0) + a) 0.0 (0.0 : map (fromIntegral . digitToInt) xs)
  in char '.' *> (frcOfStr <$> ((:) <$> digit <*> many digit)) <|> return 0.0
expPart =
  let intOfDigits = intOfStr <$> ((:) <$> digit <*> many digit)
  in ((char 'e' <|> char 'E')
       *> (char '+' *> intOfDigits
           <|> (char '-' *> ((* (-1)) <$> intOfDigits))
           <|> intOfDigits)) <|> return 0.0
intPart = intOfStr <$> ((:) <$> oneOf "123456789" <*> many digit)
          <|> (char '0' *> return 0.0)
signPart = (char '-' *> return (-1.0)) <|> return 1.0
numParser =
  let numOf [x,y,z,w] =
        let expPart n res = if n == 0 then res
                            else if n > 0 then expPart (n - 1) (res * 10.0)
                                 else expPart (n + 1) (res / 10.0)
        in JNumber (x * (y + z) * (expPart w 1.0))
  in numOf <$> sequence [signPart,intPart,fracPart,expPart] 

parenParser :: Char -> Char -> [Char] -> ParsecT [Char] u Identity v -> ParsecT [Char] u Identity [v]
parenParser st_char ed_char sep_lst psr =
  let parenS = char st_char
      parenE = char ed_char *> return []
      sep = sequence . (map char) $ sep_lst
      loop = (:) <$> psr <*> (parenE <|> (sep *> loop))
  in (try (parenS *> parenE)) <|> (parenS *> loop)

escChar =
  let hexPart = let hex = oneOf "0123456789ABCDEFabcdef"
                    charOfHex xs = chr $ foldl (\a b -> 16 * a + b) 0 $ map digitToInt xs
                in char 'u' *> (charOfHex <$> sequence [hex,hex,hex,hex])
  in char '\\'
     *> (hexPart <|> (char '"' *> return '"')
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

nullParser = string "null" *> return JNull

valParser = spaces
            *> (JStrOfVal <$> strParser
                <|> numParser <|> objParser <|> arrParser
                <|> boolParser <|> nullParser)
            <* spaces

main = do
  js <- readFile "./test.json"
  setLocaleEncoding utf8
  print $ parse valParser "" js
