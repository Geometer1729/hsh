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
import Data.Function
import SubUtils

handleLine :: String -> IO CmdReturn
handleLine = contextHandleLine def

contextHandleLine :: Context -> String -> IO CmdReturn
contextHandleLine _ "" = return def
contextHandleLine _ ('#':_) = return def
contextHandleLine context input = case parseLine input of
  Nothing -> putStrLn "syntax error" >> return def{succes=False} 
  Just line -> do
    print line
    contextHandleLineData context line

contextHandleLineData :: Context -> Line -> IO CmdReturn
contextHandleLineData _ (Extract var cmd) = undefined
contextHandleLineData _ (Let left right) = letFunc left right
contextHandleLineData c (Plain cmd) = contextHandleCmd c cmd

contextHandleCmd :: Context -> Command -> IO CmdReturn
contextHandleCmd context (Exec cmd args) = runVarOrExec cmd args context
contextHandleCmd context (Pipe cl cr) = do
  (readEnd,writeEnd) <- createPipe
  let lcontext = context{stout = Just writeEnd,wait = PassHandles}
  let rcontext = context{stin  = Just readEnd ,wait = PassHandles}
  lret <- contextHandleCmd lcontext cl
  rret <- contextHandleCmd rcontext cr
  let handles = awaits lret ++ awaits rret
  case wait context of
    Do -> do
      awaitSucs <- mapM waitForProcess handles
      return $ lret <> rret <> def{succes = and . map (== ExitSuccess) $ awaitSucs}
    Dont -> return $ (lret <> rret){awaits = [] }
    PassHandles -> return $ lret <> rret
contextHandleCmd context (Background cmd) = contextHandleCmd  context{wait=Dont} cmd
contextHandleCmd context (ITE i t e) = do 
  iret <- contextHandleCmd context i
  if shellExit iret then return def{shellExit=True} else contextHandleCmd context (if succes iret then t else e)
contextHandleCmd context (Or l r) = do
  lret <- contextHandleCmd context l
  if (succes lret) || (shellExit lret) then return lret else do 
      rret <- contextHandleCmd context r
      return $ (lret <> rret) { succes = succes lret || succes rret}
contextHandleCmd context (And l r) = do
  lret <- contextHandleCmd context l
  if (not . succes $ lret) || (shellExit $ lret) then return lret else do 
      rret <- contextHandleCmd context r
      return $ lret <> rret
contextHandleCmd context (Seq l r) = on (liftM2 (<>)) (contextHandleCmd context) l r



execOrBuiltin :: String -> [String] -> Context -> IO CmdReturn
execOrBuiltin cmd rawArgs context = do
  args' <- mapM deTildify rawArgs
  case (cmd,args') of
    ("exit",_)       -> return def{shellExit=True}
    ("cd",args)      -> fromSuc $ cd args 
    ("print",args)   -> fromSuc $ printEnvVars args 
    ("lineMap",args) -> lineMap args context
    (".",args)       -> runFiles args
    (cmd,args)       -> runExec cmd args context

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
      contextHandleLine context output' 
  Just string  -> do
    contextHandleLine context (string ++ " " ++ unwords args) 
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


--script Utils

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
