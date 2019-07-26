{-# LANGUAGE LambdaCase #-}
import CmdHandle
import Completer
import System.Environment
import System.Posix.User
import System.Posix.Directory
import System.Console.Readline hiding( getPrompt )
import Control.Monad
import Data.Maybe
import Types
import SubUtils
import ScriptUtil


main :: IO()
main = do
  args <- getArgs
  runFiles args
  when (null args) (completerInit >> hshrc >> loop) 

loop :: IO ()
loop = do
  prompt <- getPrompt
  input <- readline prompt
  when (isJust input) (do
    let line = fromJust input
    nonExit <-fmap (not . shellExit) $ handleLine line
    addHistory line
    when nonExit loop)
  

getPrompt :: IO String
getPrompt = do
  name <- getEffectiveUserName
  isRoot <- fmap (== 0) getRealUserID
  host <- fmap init $ readFile "/etc/hostname"
  pwd <- getWorkingDirectory 
  pwd' <- tildify pwd
  -- this should eventually be read from a config
  let prompt = (color 32 name) ++ "@" ++ (color 33 host) ++ ":" ++ (color 36 pwd') ++ (if isRoot then "#" else "$") ++ " "
  return prompt 

color :: Int -> String -> String
color n s = "\ESC[01;" ++ show n ++ "m" ++ s ++ "\ESC[0m"

hshrc :: IO ()
hshrc = do
  home <- lookupEnv "HOME"
  case home of
    Nothing -> return ()
    Just path -> runFile True (path ++ "/.hshrc") >> return ()
