{-# LANGUAGE LambdaCase #-}
module CmdHandle where

import Exec
import Control.Monad
import Data.Default
import Data.Function
import Parse
import System.Environment
import System.Process
import Types

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
contextHandleLineData _ (Extract var cmd) = do
  (ret,val) <- eval (Plain cmd)
  setEnv var (unwords val)
  return ret
contextHandleLineData _ (Let left right) = letFunc left right
contextHandleLineData c (Plain cmd) = contextHandleCmd c cmd

contextHandleCmd :: Context -> Command -> IO CmdReturn
contextHandleCmd context (Exec cmd args) = doExec cmd args context
contextHandleCmd context (Pipe out err cl cr) = do
  (readEnd,writeEnd) <- createPipe
  let lout = if out then Just writeEnd else Nothing
  let lerr = if err then Just writeEnd else Nothing
  let lcontext = context{stout = lout,sterr = lerr,wait = PassHandles}
  let rcontext = context{stin  = Just readEnd ,wait = PassHandles}
  lret <- contextHandleCmd lcontext cl
  rret <- contextHandleCmd rcontext cr
  case wait context of
    Do -> do
      lret' <- withWaits lret
      rret' <- withWaits rret
      return $ lret' <> rret'
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
