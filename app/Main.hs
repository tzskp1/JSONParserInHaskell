import Text.Parsec
import Data.Functor.Identity

data JNum = Int | Float deriving Show
data JStr = Char | JHex Char Char Char Char | String deriving Show
data JValue =
    JString JStr
  | JNumber JNum
  | JObject [(String,JValue)]
  | JArray [JValue]
  | JTrue
  | JFalse
  | JNull
  deriving Show

digits1_9 = foldl (<|>) (parserFail "") (map char "123456789")
hex = foldl (<|>) (parserFail "") (map char "0123456789ABCDEFabcdef")

fracPart :: ParsecT [Char] u Data.Functor.Identity.Identity [Char]
fracPart = (:) <$> char '.' <*> many digit
-- なぜか型注釈がないとコンパイルエラーになる
expPart :: ParsecT [Char] u Data.Functor.Identity.Identity [Char]
expPart = (:) <$> (char 'e' <|> char 'E') <*>
          (((:) <$> char '+' <*> many digit) <|> 
           ((:) <$> char '-' <*> many digit) <|>
           many digit)
intPart = sequence [char '0'] <|>
          ((:) <$> digits1_9 <*> (many digit))
plusPart = (++) <$> intPart <*>
           ((++) <$> (fracPart <|> sequence [])
           <*> (expPart <|> sequence []))
minusPart = (:) <$> char '-' <*> plusPart
numParser = spaces *> (minusPart <|> plusPart) <* spaces
            

strParenS = spaces *> sequence [char '"']
strParenE = sequence [char '"'] <* spaces 
hexPart = char 'u' *> sequence [hex,hex,hex,hex]
escPart = char '\\' *> hexPart
ctlPart = escPart
  
strPart = (++) <$>
  (escPart <|> ctlPart <|> sequence [anyChar]) <*>
  (strParenE <|> strPart)

strParser = (try ((++) <$> strParenS <*> strParenE)) <|>
            ((++) <$> strParenS <*> strPart)

arrParenS = spaces *> sequence [char '[']
arrParenE = sequence [char ']'] <* spaces 
arrPart = (++) <$> valParser <*>
          (arrParenE <|> ((:) <$> char ',' <*> arrPart))
arrParser = (try ((++) <$> arrParenS <*> arrParenE)) <|>
            ((++) <$> arrParenS <*> arrPart)

objParenS = spaces *> sequence [char '{']
objParenE = sequence [char '}'] <* spaces 
pairPart = (++) <$> strParser <*>
          ((:) <$> char ':' <*> valParser)
objPart = (++) <$> pairPart <*>
          (objParenE <|>
           ((:) <$> char ',' <*> objPart))
objParser = (try ((++) <$> objParenS <*> objParenE)) <|>
            ((++) <$> objParenS <*> objPart)

boolParser = string "true" <|> string "false"

valParser = try strParser <|> try numParser <|>
            try objParser <|> try arrParser <|>
            boolParser <|> string "null"
-- jsonParser = (many space) >> (char '{')

main = do
  js <- readFile "./test.json"
  print $ parse valParser "" js
