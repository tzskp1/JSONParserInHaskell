import Text.Parsec
import Data.Functor.Identity
import Data.Char
import GHC.IO.Encoding

data Format =
    JHex String
  | JNml String
  | JCtl Char
  | JQT
  | JRS
  | JSL
  | JBS
  | JFF
  | JNL
  | JCR
  | JHT
  deriving Show
data JStr = JStr [Format] deriving Show

data JValue =
    JStrAsVal JStr
  | JNumber (Int,Int,Int)        -- int,frac,exp
  | JObject [(JStr,JValue)]
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
numParser = numOf <$> sequence [intPart,fracPart,expPart]

parenParser :: Char -> Char -> [Char] -> ParsecT [Char] u Identity v -> ParsecT [Char] u Identity [v]
parenParser st_char ed_char sep_lst psr =
  let parenS = char st_char
      parenE = char ed_char *> return []
      loop = (:) <$> psr <*> (parenE <|> ((sequence . (map char)) sep_lst *> loop))
  in (try (parenS *> parenE)) <|> (parenS *> loop)

hexPart = let hex = oneOf "0123456789ABCDEFabcdef"
          in char 'u' *> (JHex <$> sequence [hex,hex,hex,hex])
-- ctlList = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\BS\HT\LF\VT\FF\CR\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US\SP\DEL"
qtPart = char '"' *> return JQT
rsPart = char '\\' *> return JRS
slPart = char '/' *> return JSL
bsPart = char 'b' *> return JBS
ffPart = char 'f' *> return JFF
nlPart = char 'n' *> return JNL
crPart = char 'r' *> return JCR
htPart = char 't' *> return JHT
escPart = char '\\'
          *> (hexPart
              <|> qtPart
              <|> rsPart
              <|> slPart
              <|> bsPart
              <|> ffPart
              <|> nlPart
              <|> crPart
              <|> htPart)
-- ctlPart = JCtl <$> oneOf ctlList 
nmlCond x = not (or (('"' == x) : ('\\' == x) : []))  --(map (== x) ctlList)))
nmlPart = JNml <$> (many $ satisfy nmlCond)
strParser = JStr <$> (parenParser '"' '"' [] (escPart <|> ctlPart <|> nmlPart))

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

main = do
  setLocaleEncoding utf8
  js <- readFile "./test.json"
  print $ parse valParser "" js
