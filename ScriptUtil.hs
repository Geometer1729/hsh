module ScriptUtil where

import {-# Source #-} CmdHandle
import Types
import Data.Default
import System.Directory
import Control.Monad

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
