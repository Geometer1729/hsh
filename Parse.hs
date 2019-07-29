{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parse where

import Text.ParserCombinators.ReadP hiding (many)
import Types
import Control.Applicative
import Control.Monad
import Data.List
import Data.Bits

import Debug.Trace

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
  left <- parseArgsLet
  eatSpace
  string "="
  right <- parseArgsLet
  maybeEatSpace
  return $ Let left right

parsePlain :: ReadP Line
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
infixes = ["||","&&",">>",">>=","1>>=","2>>=","1&2>>="]

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
  ">>=" -> Pipe True False l r
  "1>>=" -> Pipe True False l r
  "2>>=" -> Pipe False True l r
  "1&2>>=" -> Pipe True True l r
  _ -> error "unsupoorted infix string givent to liftInfix in Parse.hs"
liftInfix _ = error "liftInfix called on non infix in Parse.hs"

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
    eatSpace
    char  '&'
    rest <- look
    when ( (not $ null rest) && (head rest == '&') ) pfail
    return $ Background cmd) <++ parseExec

parseExec :: ReadP Command
parseExec =  parseParen <++ do
  name <- parseHead
  args <- parseArgs
  return $ Exec name args

parseParen :: ReadP Command
parseParen = do
  char '('
  str <- look
  when (singleWordInParen str) pfail
  when (str == "" || head str == '\\') pfail
  _ <- fmap (trace "this ran") look
  cmd <- parseCmd
  char ')'
  return cmd

singleWordInParen :: String -> Bool
singleWordInParen w = let tilParen = fst . break (== ')') $ w in not $ ' ' `elem` tilParen

parseArgs :: ReadP [String]
parseArgs = many parseArg

parseArg :: ReadP String
parseArg = (eatSpace >> parseWord)

parseArgsLet :: ReadP [String]
parseArgsLet = many (eatSpace >> munch1 (/= ' '))

parseWord :: ReadP String
parseWord = tic <++ quote1 <++ quote2  <++ ((do
    word <- munch (/= ' ')
    when (unacceptable word) pfail
    return word ) <|> (do
      word <- munch (\x -> not $ x `elem` " )")
      when (unacceptable word) pfail
      return word))

wraped :: String -> Char -> ReadP String
wraped cl cr = do
  string cl
  fmap (cl ++) (continue cr)

wrapedSimple :: Char -> ReadP String
wrapedSimple c = wraped [c] c

parseHead :: ReadP String
parseHead = wraped "(\\" ')' <++ parseWord

continue :: Char -> ReadP String
continue c = do
  n <- get
  if n == c then return [c] else do
    if n == '\\' then do
      n2 <- get
      rest <- continue c
      return ([n,n2] ++ rest)
    else do
      fmap (n:) (continue c)

    
tic , quote1 , quote2 :: ReadP String
tic    = wrapedSimple '`'
quote1 = wrapedSimple '\''
quote2 = wrapedSimple '"'

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
