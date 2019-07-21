
import System.Console.Readline
import System.Directory
import System.Environment
import Control.Monad
import Data.Maybe

-- main = do
--   setAttemptedCompletionFunction (Just completer)
--   string <- readline "test$"
--   print string

main = do
  b <- executables
  print b

completer :: String -> Int -> Int -> IO (Maybe (String,[String]))
completer word wordStart wordEnd = do
  --print (word,wordStart,wordEnd)
  return $ Just (word ++ "appended",[show n | n <- [1..100]])

executables :: IO [String]
executables = do
  maybePath <- lookupEnv "PATH"
  let path = concat . map splitPath . maybeToList $ maybePath
  goodPaths <- fmap concat $ mapM executablesIn path
  let execNames = map (last . splitSlash) goodPaths
  return execNames

executablesIn :: String -> IO [String]
executablesIn path = do
  valid <- doesDirectoryExist path
  files <- if valid then listDirectory path else return []
  let paths = [ path ++ "/" ++ file | file <- files ]
  realPaths <- filterM doesFileExist paths
  filterM isExecutable realPaths

isExecutable :: String -> IO Bool
isExecutable string = fmap executable $ getPermissions string
  

splitPath = splitOn ':'
splitSlash = splitOn '/'

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = let (x,xs) = break (== c) s in case xs of
  "" -> [x]
  s -> x: (splitPath . tail $ s)

