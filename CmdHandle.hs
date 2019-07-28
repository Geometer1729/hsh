{-# LANGUAGE LambdaCase #-}
module CmdHandle where

import ScriptUtil
import BuiltIns

import Control.Monad
import Data.Default
import Data.Function
import Data.Maybe
import SubUtils
import System.Directory
import System.Environment
import System.Exit
import System.Process
import Types
import Export
import Parse

handleLine :: String -> IO CmdReturn
handleLine = contextHandleLine def

contextHandleLine :: Context -> String -> IO CmdReturn
contextHandleLine _ "" = return def
contextHandleLine _ ('#':_) = return def
contextHandleLine context input = case parseLine input of
  Nothing -> putStrLn ( "syntax error " ++ show input ) >> return def{succes=False} 
  Just line -> do
    when debug $ print line
    contextHandleLineData context line

contextHandleLineData :: Context -> Line -> IO CmdReturn
contextHandleLineData _ (Extract var cmd) = undefined
contextHandleLineData _ (Let left right) = letFunc left right
contextHandleLineData c (Plain cmd) = contextHandleCmd c cmd

contextHandleCmd :: Context -> Command -> IO CmdReturn
contextHandleCmd context (Exec cmd args) = doExec cmd args context
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
contextHandleCmd _ (Infix _ _ _) = error "unexpaned infix passed to context handle cmd"

-- exec

type Attempt = String -> [String] -> Context -> IO (Maybe (IO CmdReturn))

seqAttempts :: [Attempt] -> Attempt
seqAttempts (x:xs) a b c = do
  r1 <- x a b c
  case r1 of
    Just ret -> return . Just $ ret
    Nothing -> seqAttempts xs a b c
seqAttempts [] _ _ _ = return $ Nothing

doExec :: String -> [String] -> Context -> IO CmdReturn
doExec name args0 context = do
  args1 <- mapM deTildify args0
  args2 <- fmap concat $ mapM glob args1
  r <- findExec name args2 context
  case r of
    Just x -> x
    Nothing -> putStrLn ("no such command could be found " ++ name) >> return def{succes=False}

findExec :: Attempt
findExec = seqAttempts [tryBuiltin,tryVar,tryExec,tryHask]

tryBuiltin :: String -> [String] -> Context -> IO (Maybe (IO CmdReturn))
tryBuiltin cmd rawArgs context = case (cmd,rawArgs) of
    ("exit",_)       -> return . Just $ return def{shellExit=True}
    ("cd",args)      -> return . Just $ fromSuc $ cd args 
    ("print",args)   -> return . Just $ fromSuc $ printEnvVars args 
    ("lineMap",args) -> return . Just $ lineMap args context
    (".",args)       -> return . Just $ runFiles args
    ("True",[])      -> return . Just $ true
    ("False",[])     -> return . Just $ false
    _                -> return $ Nothing

fromSuc :: IO Bool -> IO CmdReturn
fromSuc = fmap (\s -> def{succes=s})

tryVar :: String -> [String] -> Context -> IO (Maybe (IO CmdReturn))
tryVar path args context = lookupEnv path >>= \case
  Just ('\\':string) -> let
    ws = words string
    vars = takeWhile (/= "->") ws
    output = unwords . tail . dropWhile (/= "->") $ ws
    subs = zip vars args
    extraArgs = drop (length vars) args
    output' = dosubs subs (output ++ unwords extraArgs)
    in do
      return . Just $ contextHandleLine context output' 
  Just string  -> do
    return . Just $ contextHandleLine context (string ++ " " ++ unwords args) 
  Nothing -> return Nothing

tryExec :: String -> [String] -> Context -> IO (Maybe (IO CmdReturn))
tryExec path args context = do
  localEnv <- getEnvironment
  fromPath <- findExecutables path
  localCwd <- getCurrentDirectory
  path' <- deTildify path
  fromLocal <- findExecutablesInDirectories ["",localCwd] path'
  args' <- mapM deTildify args
  let execName = listToMaybe (fromPath ++ fromLocal)
  case execName of
    Just location -> do
      procHandle <- runProcess location args' Nothing (Just localEnv) (stin context) (stout context) (sterr context)
      -- if not awaiting the return code should be ignored
      case wait context of
        Do -> do
          suc <- fmap (== ExitSuccess) $ waitForProcess procHandle 
          return . Just . return $ CmdReturn False suc [] 
        Dont -> do
          return . Just . return $ CmdReturn False True [] 
        PassHandles -> do
          return . Just . return $ CmdReturn False True [procHandle] 
    Nothing -> return Nothing

tryHask :: String -> [String] -> Context -> IO (Maybe (IO CmdReturn))
tryHask func args context = return $ tryExport (func:args) context

