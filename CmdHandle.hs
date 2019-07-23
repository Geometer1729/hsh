{-# LANGUAGE LambdaCase #-}
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

-- IO (Bool,Bool) is shell continue , cmd succes

data Context = Context {
   wait :: Bool 
  ,stin  :: Maybe Handle 
  ,stout :: Maybe Handle
  ,sterr :: Maybe Handle
}

handleLine :: String -> IO Bool
handleLine "" = return True
handleLine ('#':_) = return True
handleLine input = case parseLine input of
  Nothing -> putStrLn "syntax error" >> return True 
  Just line -> handleLineData line

handleLineData :: Line -> IO Bool
handleLineData line = do
  print line
  contextHandleLine defContext line

contextHandleLine :: Context -> Line -> IO Bool
contextHandleLine _ (Extract var cmd) = undefined
contextHandleLine _ (Let left right) = letFunc left right
contextHandleLine c (Plain cmd) = fmap fst $ contextHandleCmd c cmd

contextHandleCmd :: Context -> Command -> IO (Bool,Bool)
contextHandleCmd context (Exec cmd args) = runVarOrExec cmd args context
contextHandleCmd context (Pipe cl cr) = do
  (readEnd,writeEnd) <- createPipe
  let lcontext = context{stout = Just writeEnd}
  let rcontext = context{stin  = Just readEnd }
  (lexit,lsuc) <- contextHandleCmd lcontext cl
  (rexit,rsuc) <- contextHandleCmd rcontext cr
  return (lexit || rexit , lsuc && rsuc ) --pipe succesfull iff both succed
contextHandleCmd context (Background cmd) = contextHandleCmd  context{wait=False} cmd
contextHandleCmd context (ITE i t e) = do -- make pipes respect this better
  (ex,r) <- contextHandleCmd context i
  if (not ex) then contextHandleCmd context (if r then t else e) else return (False,True)
contextHandleCmd context (Or l r) = do
  (v,ex) <- contextHandleCmd context l
  undefined

defContext :: Context
defContext = Context True Nothing Nothing Nothing

execOrBuiltin :: String -> [String] -> Context -> IO (Bool,Bool)
execOrBuiltin cmd rawArgs context = do
  args' <- mapM deTildify rawArgs
  case (cmd,args') of
    ("exit",_)     -> return (False,True)
    ("cd",args)    -> withoutExit $ cd args 
    ("print",args) -> withoutExit $ printEnvVars args 
    (".",args)     -> runFiles args
    (cmd,args)     -> withoutExit $ runExec cmd args context

withoutExit :: IO Bool -> IO (Bool,Bool)
withoutExit = fmap ((,) True)

runVarOrExec :: String -> [String] -> Context -> IO (Bool,Bool)
runVarOrExec path args context = lookupEnv path >>= \case
  Just ('\\':string) -> let
    ws = words string
    vars = takeWhile (/= "->") ws
    output = unwords . tail . dropWhile (/= "->") $ ws
    subs = zip vars args
    extraArgs = drop (length vars) args
    output' = dosubs subs (output ++ unwords extraArgs)
    in do
      x <- handleLine output' 
      return (x,True)
  Just string  -> do
    x <- handleLine (string ++ " " ++ unwords args)
    return (x,True)
  Nothing -> execOrBuiltin path args context
  


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

dosubs :: [(String,String)] -> String -> String
dosubs [] s = s
dosubs ((m,r):xs) s = dosubs xs (sub m r s)

runFiles :: [String] -> IO (Bool,Bool)
runFiles [] = return (True,True)
runFiles (x:xs) = do
  (l,r) <- runFile False x
  if l then do
    (l2,r2) <- runFiles xs
    return (True,r && r2)
  else do
    return (False,r)

runFile :: Bool -> String -> IO (Bool,Bool)
runFile silent path = do
 valid <- doesFileExist path
 if valid then do
   contents <- readFile path
   let ls = lines contents
   exited <- runLines ls
   return (exited,True)
  else when (not silent) (putStrLn "file not found") >> return (True,False)

runLines :: [String] -> IO Bool
runLines [] = return True
runLines (x:xs) = do
  l <- handleLine x
  if l then do
    runLines xs
  else do
    return False


