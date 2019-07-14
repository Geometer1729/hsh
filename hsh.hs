{-# LANGUAGE LambdaCase #-}
import CmdHandle
import Parse
import System.Environment
import System.IO
import System.Posix.User

main = do
  setEnv "SHELL" "/bin/hsh"
  continue <- (prompt >> getLine >>= handleLine)
  if continue then main else return ()
  
handleLine :: String -> IO Bool
handleLine "" = return True
handleLine input = case parseCommand input of
  Nothing -> putStrLn "syntax error" >> return True 
  Just cmd -> handleCmd cmd

prompt :: IO ()
prompt = do
  name <- getEffectiveUserName
  isRoot <- fmap (== 0) getRealUserID
  host <- fmap init $ readFile "/etc/hostname"
  pwd <- getEnv "PWD"
  pwd' <- tildify pwd
  -- this should eventually be read from a config
  let prompt = (color 32 name) ++ "@" ++ (color 33 host) ++ ":" ++ (color 36 pwd') ++ (if isRoot then "#" else "$") ++ " "
  putStr prompt 
  hFlush stdout

color :: Int -> String -> String
color n s = "\ESC[01;" ++ show n ++ "m" ++ s ++ "\ESC[0m"

