module Parse where
  
import Text.ParserCombinators.ReadP hiding (many)
import Control.Applicative 

data Command = Extract String Command | Pipe Command Command | Exec String [String] | Background Command | Write Command String deriving(Show)

parseCommand :: String -> Command
parseCommand s = let parser = readP_to_S cmdParser
                     parses = parser s
                     finished = filter ((=="") . snd) parses
                in if length finished == 1
                    then fst . head $ finished
                    else error $ show parses
cmdParser :: ReadP Command
cmdParser = parseExtract <++ parseSubExtract

parseSubExtract :: ReadP Command
parseSubExtract = parsePipe <++ parseSubPipe

parseSubPipe :: ReadP Command
parseSubPipe = parseExec

parseExec :: ReadP Command
parseExec = do
  exec <- parseWord
  args <- parseArgs
  return $ Exec exec args

parseWord :: ReadP String
parseWord = munch (/= ' ')

parseArgs :: ReadP [String]
parseArgs = many (char ' ' >> parseWord)

parseExtract :: ReadP Command
parseExtract = do
  var <- parseWord
  string " <- "
  cmd <- parseSubExtract
  return $ Extract var cmd

parsePipe :: ReadP Command
parsePipe = do
  cmd1 <- parseSubPipe
  string " >>= "
  cmd2 <- parseSubExtract
  return $ Pipe cmd1 cmd2

