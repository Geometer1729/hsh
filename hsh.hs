{-# LANGUAGE LambdaCase #-}
import System.Process
import System.Environment
import System.Directory
import Control.Monad

main = do
  continue <- (getLine >>= handleLine)
  if continue then main else return ()
  

handleLine :: String -> IO Bool
handleLine input = case words input of
  [] -> return True
  ("exit":_) -> return False
  ("cd":args) -> cd args >> return True
  ("print":args) -> printEnvVars args >> return True
  (cmd:args) -> runCmd cmd args >> return True

printEnvVars :: [String] -> IO [()]
printEnvVars = mapM printEnvVar 

printEnvVar :: String -> IO ()
printEnvVar var = do
  result <- lookupEnv var
  case result of
    Just value -> putStrLn (var ++ " " ++ value)
    Nothing -> putStrLn ("variable " ++ var ++ " is not set")

cd :: [String] -> IO ()
cd [] = lookupEnv "HOME" >>= \case 
  Nothing -> putStrLn "error HOME not set"
  Just path -> cd (path:[])
cd (dir:[]) = do
  pwd <- lookupEnv "PWD"
  if head dir == '/' then 
    tryCd dir
    else case pwd of 
      Just wd -> tryCd (wd ++ dir)
      Nothing -> putStrLn "PWD not set and relative path given"
cd (a:b:_) = putStrLn "too many args to cd"

tryCd :: String -> IO ()
tryCd path = do
  valid <- doesDirectoryExist path
  if valid then setEnv "PWD" path
  else do
    isFile <- doesFileExist path
    if isFile then
        putStrLn (path ++ " is a file not a directory")
    else
        putStrLn ("no such file or directory")




runCmd :: String -> [String] -> IO ()
runCmd cmd args = do
  env <- getEnvironment
  procHandle <- runProcess cmd args Nothing (Just env) Nothing Nothing Nothing
  exitCode <- waitForProcess procHandle
  return ()
