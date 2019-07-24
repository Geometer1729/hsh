{-# LANGUAGE LambdaCase #-}
module BuiltIns where

import Control.Monad
import Data.Default
import System.Directory
import System.Environment
import System.IO
import System.Posix.Directory
import System.Process
import System.Exit
import Types

import {-# Source #-} CmdHandle

cd :: [String] -> IO Bool
cd [] = lookupEnv "HOME" >>= \case 
  Nothing -> putStrLn "error HOME not set" >> return False
  Just path -> tryCd path
cd (dir:[]) = do
  pwd <- lookupEnv "PWD"
  if head dir == '/' then 
    tryCd dir
    else case pwd of 
      Just wd -> tryCd (wd ++ "/" ++ dir)
      Nothing -> putStrLn "PWD not set and relative path given" >> return False
cd (a:b:_) = putStrLn "too many args to cd" >> return False

tryCd :: String -> IO Bool
tryCd path = do
  valid <- doesDirectoryExist path
  if valid then do
    canonPath <- canonicalizePath path
    setEnv "PWD" canonPath
    changeWorkingDirectory canonPath
    return True
  else do
    isFile <- doesFileExist path
    if isFile then 
      putStrLn (path ++ " is a file not a directory") >> return False
    else 
      putStrLn ("no such file or directory") >> return False

letFunc :: [String] -> [String] -> IO CmdReturn
letFunc left right = let
    value = case left of
      [var] -> unwords right
      (func:args) -> "\\" ++ (unwords . tail $ left) ++ " -> " ++ unwords right
    in setEnv (head left) value >> return def

printEnvVars :: [String] -> IO Bool
printEnvVars vars = fmap and $ mapM printEnvVar vars

printEnvVar :: String -> IO Bool
printEnvVar var = do
  result <- lookupEnv var
  case result of
    Just value -> putStrLn (var ++ "=" ++ value) >> return True
    Nothing -> putStrLn ("variable " ++ var ++ " is not set") >> return False

lineMap :: [String] -> Context -> IO CmdReturn
lineMap (f:cmd) context = do
  (readEnd,writeEnd) <- createPipe
  cmdRet <- contextHandleLine context{stout = Just writeEnd,wait=PassHandles} (unwords cmd)
  lineRets <- lineMapLoop f readEnd context
  awaitSuc <-  mapM waitForProcess (awaits cmdRet)
  let cmdRet' = cmdRet <> def{succes=and . map  (== ExitSuccess) $ awaitSuc}
  return $ mconcat (cmdRet':lineRets)


lineMapLoop :: String -> Handle -> Context -> IO [CmdReturn]
lineMapLoop f h context = do
  line <- hGetLine h
  r <- contextHandleLine context (f ++ " " ++ line)
  done <- hIsEOF h
  if done then return [r] else do
    fmap (r:) $ lineMapLoop f h context

true :: IO CmdReturn
true = return def

false :: IO CmdReturn
false = return def{succes=False}
