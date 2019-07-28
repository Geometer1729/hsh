module Completer where

import System.Console.Haskeline
import System.Directory
import System.Environment
import Control.Monad
import Data.Maybe
import Data.List
import SubUtils

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
compType _  = File

simpleComp :: String -> Completion
simpleComp s = Completion s "" True

compExec :: String -> IO [Completion]
compExec word = do
  execs <- executables
  let opts = filter (isPrefix word) execs
  return $ map simpleComp opts

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

