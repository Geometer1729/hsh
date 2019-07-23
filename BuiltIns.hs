{-# LANGUAGE LambdaCase #-}
module BuiltIns where

--import System.Process
import System.Environment
import System.Directory
import System.Posix.Directory
import Control.Monad

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

letFunc :: [String] -> [String] -> IO Bool
letFunc left right = let
    value = case left of
      [var] -> unwords right
      (func:args) -> "\\" ++ (unwords . tail $ left) ++ " -> " ++ unwords right
    in setEnv (head left) value >> return False

    


printEnvVars :: [String] -> IO Bool
printEnvVars vars = fmap and $ mapM printEnvVar vars

printEnvVar :: String -> IO Bool
printEnvVar var = do
  result <- lookupEnv var
  case result of
    Just value -> putStrLn (var ++ "=" ++ value) >> return True
    Nothing -> putStrLn ("variable " ++ var ++ " is not set") >> return False

