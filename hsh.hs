{-# LANGUAGE LambdaCase #-}
import System.Process
import System.IO
import System.Environment
import System.Directory
import System.Posix.Directory
import System.Posix.User
import Control.Monad
import Data.Maybe
import Data.Text (pack,replace)
import CD (cd)

main = do
  prompt
  continue <- (getLine >>= handleLine)
  if continue then main else return ()
  

handleLine :: String -> IO Bool
handleLine input = do
  let args = words input
  args' <- mapM deTildify args
  case args' of
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

prompt :: IO ()
prompt = do
  name <- getEffectiveUserName
  isRoot <- fmap (== 0) getRealUserID
  host <- fmap init $ readFile "/etc/hostname"
  pwd <- getEnv "PWD"
  pwd' <- tildify pwd
  let prompt = (color 32 name) ++ "@" ++ (color 33 host) ++ ":" ++ (color 36 pwd') ++ (if isRoot then "#" else "$") ++ " "
  putStr prompt 
  hFlush stdout

color :: Int -> String -> String
color n s = "\ESC[01;" ++ show n ++ "m" ++ s ++ "\ESC[0m"

deTildify :: String -> IO String
deTildify  s = do
  home <- getEnv "HOME"
  return $ sub "~" home s

tildify :: String -> IO String
tildify s = do
  home <- getEnv "HOME"
  return $ sub home "~" s
  
sub :: String -> String -> String -> String
sub _ _ [] = []
sub match replace input = if isPrefix then replace ++ (sub match replace (drop (length match) input)) else (head input) : (sub match replace (tail input))
  where
    isPrefix = (length match <= length input) && (and $ zipWith (==) match input)
