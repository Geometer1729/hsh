import CmdHandle
import Completer
import Control.Monad
import Control.Monad.Trans
import ScriptUtil
import SubUtils
import System.Environment
import System.Exit
import System.Posix.Directory
import System.Posix.User
import Types
import System.Console.Haskeline

main :: IO ()
main = do
  args <- getArgs
  if (null args) then
    when (null args) (hshrc >> hsh) 
  else do
    fileRet <- runFiles args
    if succes fileRet then exitSuccess else exitFailure

hsh :: IO ()
hsh = do
  prompt <- getPrompt
  runInputT settings (handle (\Interrupt -> loop prompt) $ withInterrupt $ loop prompt)  

loop :: String -> InputT IO ()
loop prompt = do
  line <- getInputLine prompt
  case line of
    Nothing -> return ()
    Just text -> do
      ret <- lift $ handleLine text
      when (not . shellExit $ ret) (loop prompt)

getPrompt :: IO String
getPrompt = do
  name <- getEffectiveUserName
  isRoot <- fmap (== 0) getRealUserID
  host <- fmap init $ readFile "/etc/hostname"
  pwd <- getWorkingDirectory 
  pwd' <- tildify pwd
  -- this should eventually be read from a config
  let prompt = (color 32 name) ++ "@" ++ (color 33 host) ++ ":" ++ (color 36 pwd') ++ (if isRoot then "#" else "$") ++ " "
  if debug then return $ "this is hsh" ++ prompt else return prompt 

color :: Int -> String -> String
color n s = "\ESC[01;" ++ show n ++ "m" ++ s ++ "\ESC[0m"

hshrc :: IO ()
hshrc = do
  home <- lookupEnv "HOME"
  case home of
    Nothing -> return ()
    Just path -> runFile True (path ++ "/.hshrc") >> return ()
