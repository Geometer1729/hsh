module Completer where

import System.Console.Readline
import System.Directory
import System.Environment
import Control.Monad
import Data.Maybe
import Data.List
import SubUtils

completerInit :: IO ()
completerInit = do
  home <- lookupEnv "HOME"
  case home of 
    Nothing -> return ()
    Just path -> do
      let inputrc = path ++ "/.inputrc"
      exists <- doesFileExist inputrc
      when exists $ readInitFile inputrc 
  setAttemptedCompletionFunction (Just completer)

completer :: String -> Int -> Int -> IO (Maybe (String,[String]))
completer word 0 _ = do
  execs <- executables
  let valid = filter (isPrefix word) execs
  case valid of
    []     -> return Nothing
    (x:[]) -> return (Just (x,[]))
    xs     -> return (Just (word,xs))
completer word _ _ = do
  path <- deTildify word
  files <- filenameCompletionFunction path
  files' <- mapM tildify files
  case files' of
    []     -> return Nothing
    (x:[]) -> return $ Just (x,[])
    _     -> return Nothing

executables :: IO [String]
executables = do
  maybePath <- lookupEnv "PATH"
  let path = concat . map splitPath . maybeToList $ maybePath
  goodPaths <- fmap concat $ mapM executablesIn path
  let execNames = map (last . splitSlash) goodPaths
  return $ map head . group . sort $ execNames

executablesIn :: String -> IO [String]
executablesIn path = do
  valid <- doesDirectoryExist path
  files <- if valid then listDirectory path else return []
  let paths = [ path ++ "/" ++ file | file <- files ]
  realPaths <- filterM doesFileExist paths
  filterM isExecutable realPaths

isExecutable :: String -> IO Bool
isExecutable string = fmap executable $ getPermissions string

splitPath,splitSlash :: String -> [String]
splitPath = splitOn ':'
splitSlash = splitOn '/'

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = let (x,xs) = break (== c) s in case xs of
  "" -> [x]
  w -> x: (splitOn c . tail $ w)

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix a b = and $ zipWith (==) a b
