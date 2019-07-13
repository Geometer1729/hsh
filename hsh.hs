{-# LANGUAGE LambdaCase #-}
import System.Process
import System.Environment
import System.Directory
import System.Posix.Directory
import Control.Monad
import Data.Maybe
import CD (cd)

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

runCmd :: String -> [String] -> IO ()
runCmd path args = do
  env <- getEnvironment
  executable <- findExecutable path
  if isJust executable then do
    procHandle <- runProcess path args Nothing (Just env) Nothing Nothing Nothing
    exitCode <- waitForProcess procHandle
    return ()
  else
    putStrLn (path ++ " not found")
