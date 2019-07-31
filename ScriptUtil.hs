module ScriptUtil where

import Types
import Data.Default
import System.Directory
import Control.Monad

import {-# Source #-} CmdHandle

runFiles :: [String] -> IO CmdReturn
runFiles = fmap mconcat . mapM (runFile True) 

runFile :: Bool -> String -> IO CmdReturn
runFile silent path = do
 valid <- doesFileExist path
 if valid then do
   contents <- readFile path
   let ls = lines contents
   runLines ls
  else when (not silent) (putStrLn "file not found") >> return defRet

runLines :: [String] -> IO CmdReturn
runLines [] = return defRet
runLines (x:xs) = do
  l <- handleLine x
  if (not . shellExit $ l) then do
    runLines xs
  else do
    return l
