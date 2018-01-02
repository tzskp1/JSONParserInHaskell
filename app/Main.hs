import Text.Parsec
import Data.Functor.Identity
import Data.Char

data JValue =
    JString [[Char]]
  | JNumber (Int,Int,Int)        -- int,frac,exp
  | JObject [(JValue,JValue)]
  | JArray [JValue]
  | JTrue
  | JFalse
  | JNull
  deriving Show

intOfStr [] = 0
intOfStr xs = foldl (\a b -> 10 * a + b) 0 $ map digitToInt xs
numOf [x,y,z] = JNumber (x,y,z)
-- なぜか型注釈がないとコンパイルエラーになる
fracPart :: ParsecT [Char] u Identity Int
fracPart = ((char '.') *> (intOfStr <$> many digit))
           <|> (return 0)
expPart :: ParsecT [Char] u Identity Int
expPart = ((char 'e' <|> char 'E')
           *> (char '+' *> (intOfStr <$> many digit))
           <|> (char '-' *> (((* (-1)) . intOfStr) <$> many digit))
           <|> (intOfStr <$> many digit))
          <|> (return 0)
intPart :: ParsecT [Char] u Identity Int
intPart = (char '-' *> (((* (-1)) . intOfStr)
                        <$> ((:) <$> digit <*> many digit)))
          <|> (intOfStr <$> ((:) <$> digit <*> many digit))
numParser = spaces
            *> (numOf <$> sequence [intPart,fracPart,expPart])
            <* spaces

hex :: ParsecT [Char] u Identity Char
hex = foldl (<|>) (parserFail "") (map char "0123456789ABCDEFabcdef")
strParenS = spaces *> char '"'
strParenE = (char '"' <* spaces) *> (return [])
hexPart = char 'u' *> sequence [hex,hex,hex,hex]
escPart = char '\\' *> hexPart
ctlPart = escPart
attach v (ys:xs) = (v:ys):xs
attach v [] = [[v]]
strPart = (([]:) <$> ((:) <$> (escPart <|> ctlPart)
                      <*> (strParenE <|> strPart)))
          <|> (attach <$> anyChar <*> (strParenE <|> strPart))
strParser = JString <$> (try (strParenS *> strParenE)
                          <|> (strParenS *> strPart))

arrParenS = spaces *> char '['
arrParenE = (char ']' <* spaces) *> (return [])
arrPart = (:) <$> valParser <*>
          (arrParenE <|> (char ',' *> arrPart))
arrParser = JArray <$> (try (arrParenS *> arrParenE)
                         <|> (arrParenS *> arrPart))

objParenS = spaces *> char '{'
objParenE = (char '}' <* spaces) *> (return [])
pair x y = (x,y)
pairPart = pair <$> strParser <*> (char ':' *> valParser)
objPart = (:) <$> pairPart <*>
          (objParenE <|> (char ',' *> objPart))
objParser = JObject <$> (try (objParenS *> objParenE)
                          <|> (objParenS *> objPart))

boolParser = spaces
             *> ((string "true" *> return JTrue) <|> (string "false" *> return JFalse))
             <* spaces

valParser = try strParser <|> try numParser
            <|> try objParser <|> try arrParser
            <|> try boolParser
            <|> spaces *> (string "null" *> return JNull) <* spaces

main = do
  js <- readFile "./test.json"
  print $ parse valParser "" js
