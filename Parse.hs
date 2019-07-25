module Parse where

import Text.ParserCombinators.ReadP hiding (many)
import Types
import Control.Applicative
import Control.Monad
import Data.List
import Data.Bits

parseLine :: String -> Maybe Line
parseLine s = let 
  parser = readP_to_S (lineParser)
  parses = map head . group . sort $ parser s
  finished = filter ((=="") . snd) parses
    in if length finished == 1
      then Just . fst . head $ finished
      else Nothing

debugParse :: String -> IO ()
debugParse s = do
  let ps = readP_to_S lineParser s
  mapM_ (\(a,b) -> print a >> print b) ps

lineParser :: ReadP Line
lineParser = ignoreTrailingWhiteSpace $ parseExtract <++ parseLet <++ parsePlain

parseExtract :: ReadP Line
parseExtract = do
  var <- parseWord
  eatSpace
  string "<-"
  eatSpace
  cmd <- parseCmd
  return $ Extract var cmd

parseLet :: ReadP Line
parseLet = do
  string "let"
  left <- parseArgs
  string " ="
  right <- parseArgs
  return $ Let left right

parsePlain = fmap Plain parseCmd

{-
  - CMD grammer
    -
    - Cmd      <- nonInfix `infix` CMD | nonInfix
    - nonInfix <- if cmd then cmd else cmd | simple
    - simple   <- exec | exec &
    - exec <- path [args] | ( CMD )
    -
    -}

infixes :: [String]
infixes = ["||","&&",">>",">>="]

parseCmd :: ReadP Command
parseCmd = fmap liftInfix (do
    ni <- parseNonInfix
    eatSpace
    infixWord <- foldl (<|>) pfail (map string infixes)
    eatSpace
    cmd <- parseCmd
    return $ Infix ni infixWord cmd ) <++ parseNonInfix

liftInfix :: Command -> Command
liftInfix (Infix l word r) = case word of
  "&&"  -> And l r
  "||"  -> Or l r
  ">>"  -> Seq l r
  ">>=" -> Pipe l r

parseNonInfix :: ReadP Command
parseNonInfix = (do
    string "if " 
    i <- parseCmd
    string " then "
    t <- parseCmd
    string " else "
    e <- parseCmd
    return $ ITE i t e ) <++ parseSimple

parseSimple :: ReadP Command 
parseSimple = (do
    cmd <- parseExec
    string " &"
    rest <- look
    when ( (null rest) || (head rest == '&') ) pfail
    return $ Background cmd) <++ parseExec

parseExec :: ReadP Command
parseExec =  parseParen <++ do
  name <- parseWord
  args <- parseArgs
  return $ Exec name args

parseParen :: ReadP Command
parseParen = do
  char '('
  cmd <- parseCmd
  char ')'
  return cmd

parseArgs :: ReadP [String]
parseArgs = many parseArg

parseArg :: ReadP String
parseArg = do
  char ' '
  parseWord

parseWord :: ReadP String
parseWord = (do
    word <- munch (/= ' ')
    when (unacceptable word) pfail
    return word ) <|> (do
      word <- munch (\x -> not $ x `elem` " )")
      when (unacceptable word) pfail
      return word)

unacceptable :: String -> Bool
unacceptable s = (s `elem` unacceptableWords) || ((head s == '(') `xor` (last s == ')'))

unacceptableWords :: [String]
unacceptableWords = "" : infixes

eatSpace :: ReadP ()
eatSpace = many1 (char ' ') >> return ()

ignoreTrailingWhiteSpace :: ReadP a -> ReadP a
ignoreTrailingWhiteSpace p = do
  x <- p
  maybeEatSpace
  return x

maybeEatSpace :: ReadP ()
maybeEatSpace = many (char ' ') >> return ()
