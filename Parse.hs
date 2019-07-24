module Parse where
  
import Text.ParserCombinators.ReadP hiding (many)
import Control.Applicative 
import Types



parseLine :: String -> Maybe Line
parseLine s = let parser = readP_to_S (lineParser)
                  parses = parser s
                  finished = filter ((=="") . snd) parses
                in if length finished == 1
                    then Just . fst . head $ finished
                    else Nothing

lineParser :: ReadP Line
lineParser = ignoreTrailingWhiteSpace $ parseExtract <++ parseLet <++ parsePlain

parseExtract :: ReadP Line
parseExtract = do
  var <- parseWord
  eatSpace
  string "<-"
  eatSpace
  cmd <- cmdParser
  return $ Extract var cmd

parseLet :: ReadP Line
parseLet = do
  string "let "
  left <- parseArgs
  string "= "
  right <- parseArgs
  return $ Let left right

parsePlain = fmap Plain cmdParser

cmdParser :: ReadP Command
cmdParser = fmap applyInfix $ iteParse <++ parseInfix <++ bgParse <++ parseExec

iteParse :: ReadP Command
iteParse = do
  string "if "
  i <- cmdParser
  string " then "
  t <- cmdParser
  string " else "
  e <- cmdParser
  return $ ITE i t e

bgParse :: ReadP Command
bgParse = do
  cmd <- parseExec
  eatSpace
  string "&"
  return $ Background cmd

parseInfix :: ReadP Command
parseInfix = do
  cmd1 <- bgParse <|> parseExec -- <++ prevents parsing && 
  eatSpace
  infixStr <- foldl (<|>) pfail (map string [">>=","&&","||",">>"])
  eatSpace
  cmd2 <- cmdParser
  return $ Infix cmd1 infixStr cmd2

parseExec :: ReadP Command
parseExec = do
  exec <- parseWord
  args <- parseArgs
  return $ Exec exec args

parseWord :: ReadP String
parseWord = do
  result <- munch (/= ' ')
  if result `elem` ["","<-",">>=","&&","||","&"] then pfail else return result

parseArgs :: ReadP [String]
parseArgs = many (eatSpace >> parseWord)

eatSpace :: ReadP ()
eatSpace = (many1 $ char ' ') >> return ()

ignoreTrailingWhiteSpace :: ReadP a -> ReadP a
ignoreTrailingWhiteSpace p = do
  x <- p
  many (char ' ')
  return x

applyInfix :: Command -> Command
applyInfix (Background c) = Background (applyInfix c)
applyInfix (ITE c1 c2 c3) = ITE        (applyInfix c1) (applyInfix c2) (applyInfix c3)
applyInfix (Or c1 c2)     = Or         (applyInfix c1) (applyInfix c2)
applyInfix (And c1 c2)    = And        (applyInfix c1) (applyInfix c2)
applyInfix (Seq c1 c2)    = Seq        (applyInfix c1) (applyInfix c2)
applyInfix (Pipe c1 c2)   = Pipe       (applyInfix c1) (applyInfix c2)
applyInfix (Exec s as )   = Exec s as
applyInfix (Infix c1 s c2) = (case s of
  ">>=" -> Pipe 
  "&&"  -> And 
  "||"  -> Or
  ">>"  -> Seq ) (applyInfix c1) (applyInfix c2)


