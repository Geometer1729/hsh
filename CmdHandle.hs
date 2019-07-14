module CmdHandle where

import BuiltIns
import Control.Monad
import Data.Maybe
import Parse
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.Directory
import System.Posix.User
import System.Process

data Context = Context {
   wait :: Bool 
  ,stin  :: Maybe Handle 
  ,stout :: Maybe Handle
  ,sterr :: Maybe Handle
}

handleCmd :: Command -> IO Bool
handleCmd = contextHandleCmd defContext

contextHandleCmd :: Context -> Command -> IO Bool
contextHandleCmd context (Exec cmd args) = fmap fst $ execOrBuiltin cmd args context
contextHandleCmd context (Pipe cl cr) = do
  (readEnd,writeEnd) <- createPipe
  let lcontext = context{stout = Just writeEnd}
  let rcontext = context{stin  = Just readEnd }
  lexit <- contextHandleCmd lcontext cl
  rexit <- contextHandleCmd rcontext cr
  return (lexit && rexit) --pipe succesfull iff both succed
contextHandleCmd context (Background cmd) = contextHandleCmd  context{wait=False} cmd


defContext :: Context
defContext = Context True Nothing Nothing Nothing

execOrBuiltin :: String -> [String] -> Context -> IO (Bool,Bool)
execOrBuiltin cmd rawArgs context = do
  args' <- mapM deTildify rawArgs
  case (cmd,args') of
    ("exit",_)     -> return (False,True)
    ("cd",args)    -> withoutExit $ cd args 
    ("print",args) -> withoutExit $ printEnvVars args 
    ("let",args)   -> withoutExit $ letFunc args 
    (cmd,args)     -> withoutExit $ runExec cmd args context

withoutExit :: IO Bool -> IO (Bool,Bool)
withoutExit = fmap ((,) True)

runExec :: String -> [String] -> Context -> IO Bool
runExec path args context = do
  env <- getEnvironment
  executable <- findExecutable path
  if isJust executable then do
    procHandle <- runProcess path args Nothing (Just env) (stin context) (stout context) (sterr context)
    -- if not awaiting the return code should be ignored
    if wait context then fmap (== ExitSuccess) $ waitForProcess procHandle else return True
  else do
    putStrLn (path ++ " not found")
    return False

deTildify :: String -> IO String
deTildify  s = do
  home <- getEnv "HOME"
  return $ sub "~" home s

tildify :: String -> IO String
tildify s = do
  home <- getEnv "HOME"
  return $ sub home "~" s
  
sub :: String -> String -> String -> String
sub _ _ [] = []
sub match replace input = if isPrefix then replace ++ (sub match replace (drop (length match) input)) else (head input) : (sub match replace (tail input))
  where
    isPrefix = (length match <= length input) && (and $ zipWith (==) match input)

