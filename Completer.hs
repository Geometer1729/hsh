module Completer where

import System.Console.Haskeline
import System.Directory
import System.Environment
import Control.Monad
import Data.Maybe
import Data.List
import SubUtils
import Export
import Exec (builtins)
import Parse
import Data.Char

readLine :: String -> IO (Maybe String)
readLine prompt = runInputT settings (handleInterrupt lineIn $ withInterrupt $ lineIn)
  where
    lineIn :: InputT IO (Maybe String)
    lineIn = getInputLine prompt

settings :: Settings IO
settings = setComplete comp defaultSettings

comp :: CompletionFunc IO
comp = completeWordWithPrev Nothing " " completer

completer :: String -> String -> IO [Completion]
completer prior word = case compType prior of
    Exec -> compExec word
    File -> compFile word

data CompType = Exec | File

compType :: String -> CompType
compType "" = Exec
compType w  = let
  lw = last . words $ w
  thenExecWords = infixes ++ ["sudo","="]
  in
  if last lw == '(' || lw `elem` thenExecWords then Exec else File


compExec :: String -> IO [Completion]
compExec word = do
  execs <- executables
  let execs1 = execs ++ names --exports
  let execs2 = execs1 ++ builtins
  let (sym,_) = break isAlpha word
  let opts = filter (isPrefix word) (map (sym ++ ) execs2)
  return $ map simpleCompletion opts

compFile :: String -> IO [Completion]
compFile w = do
  w' <- deTildify w
  cs <- listFiles w'
  tildifyComps cs

tildifyComps :: [Completion] -> IO [Completion]
tildifyComps = mapM liftTildify

liftTildify :: Completion -> IO Completion
liftTildify c = do
  let r = replacement c
  r' <- tildify r
  return c{replacement=r'}

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

