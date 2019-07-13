{-# LANGUAGE LambdaCase #-}
module CD where

--import System.Process
import System.Environment
import System.Directory
import System.Posix.Directory

cd :: [String] -> IO ()
cd [] = lookupEnv "HOME" >>= \case 
  Nothing -> putStrLn "error HOME not set"
  Just path -> cd (path:[])
cd (dir:[]) = do
  pwd <- lookupEnv "PWD"
  if head dir == '/' then 
    tryCd dir
    else case pwd of 
      Just wd -> tryCd (wd ++ "/" ++ dir)
      Nothing -> putStrLn "PWD not set and relative path given"
cd (a:b:_) = putStrLn "too many args to cd"

tryCd :: String -> IO ()
tryCd path = do
  valid <- doesDirectoryExist path
  if valid then do
    canonPath <- canonicalizePath path
    setEnv "PWD" canonPath
    changeWorkingDirectory canonPath
  else do
    isFile <- doesFileExist path
    if isFile then
      putStrLn (path ++ " is a file not a directory")
    else
      putStrLn ("no such file or directory")
