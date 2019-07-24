{-# LANGUAGE LambdaCase #-}
module CmdHandle where

import BuiltIns
import Types
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
import Data.Default

handleLine :: String -> IO CmdReturn
handleLine "" = return def
handleLine ('#':_) = return def
handleLine input = case parseLine input of
  Nothing -> putStrLn "syntax error" >> return def{succes=False} 
  Just line -> handleLineData line

handleLineData :: Line -> IO CmdReturn
handleLineData line = do
  print line
  contextHandleLine def line

contextHandleLine :: Context -> Line -> IO CmdReturn
contextHandleLine _ (Extract var cmd) = undefined
contextHandleLine _ (Let left right) = letFunc left right
contextHandleLine c (Plain cmd) = contextHandleCmd c cmd

contextHandleCmd :: Context -> Command -> IO CmdReturn
contextHandleCmd context (Exec cmd args) = runVarOrExec cmd args context
contextHandleCmd context (Pipe cl cr) = do
  (readEnd,writeEnd) <- createPipe
  let lcontext = context{stout = Just writeEnd}
  let rcontext = context{stin  = Just readEnd }
  lret <- contextHandleCmd lcontext cl
  rret <- contextHandleCmd rcontext cr
  return $ lret <> rret
contextHandleCmd context (Background cmd) = contextHandleCmd  context{wait=Dont} cmd
contextHandleCmd context (ITE i t e) = do -- make pipes respect this better
  iret <- contextHandleCmd context i
  if shellExit iret then return def{shellExit=True} else contextHandleCmd context (if succes iret then t else e)
contextHandleCmd context (Or l r) = do
  undefined

execOrBuiltin :: String -> [String] -> Context -> IO CmdReturn
execOrBuiltin cmd rawArgs context = do
  args' <- mapM deTildify rawArgs
  case (cmd,args') of
    ("exit",_)     -> return def{shellExit=True}
    ("cd",args)    -> fromSuc $ cd args 
    ("print",args) -> fromSuc $ printEnvVars args 
    (".",args)     -> runFiles args
    (cmd,args)     -> runExec cmd args context

fromSuc :: IO Bool -> IO CmdReturn
fromSuc = fmap (\s -> def{succes=s})

runVarOrExec :: String -> [String] -> Context -> IO CmdReturn
runVarOrExec path args context = lookupEnv path >>= \case
  Just ('\\':string) -> let
    ws = words string
    vars = takeWhile (/= "->") ws
    output = unwords . tail . dropWhile (/= "->") $ ws
    subs = zip vars args
    extraArgs = drop (length vars) args
    output' = dosubs subs (output ++ unwords extraArgs)
    in do
      handleLine output' 
  Just string  -> do
    handleLine (string ++ " " ++ unwords args)
  Nothing -> execOrBuiltin path args context
  
runExec :: String -> [String] -> Context -> IO CmdReturn
runExec path args context = do
  env <- getEnvironment
  executable <- findExecutable path
  if isJust executable then do
    procHandle <- runProcess path args Nothing (Just env) (stin context) (stout context) (sterr context)
    -- if not awaiting the return code should be ignored
    case wait context of
      Do -> do
        suc <- fmap (== ExitSuccess) $ waitForProcess procHandle 
        return $ CmdReturn False suc [] 
      Dont -> do
        return $ CmdReturn False True [] 
      PassHandles -> do
        return $ CmdReturn False True [procHandle] 
  else do
    putStrLn (path ++ " not found")
    return $ CmdReturn False False [] 

-- sub utils

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

-- script utils

runFiles :: [String] -> IO CmdReturn
runFiles [] = return def
runFiles (x:xs) = do
  xRet <- runFile False x
  if shellExit xRet then do
    xsRet <- runFiles xs
    return $ xRet <> xsRet
  else do
    return def{shellExit = True}

runFile :: Bool -> String -> IO CmdReturn
runFile silent path = do
 valid <- doesFileExist path
 if valid then do
   contents <- readFile path
   let ls = lines contents
   runLines ls
  else when (not silent) (putStrLn "file not found") >> def

runLines :: [String] -> IO CmdReturn
runLines [] = return def
runLines (x:xs) = do
  l <- handleLine x
  if (not . shellExit $ l) then do
    runLines xs
  else do
    return l


