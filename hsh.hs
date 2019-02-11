import SysCall

import System.Environment
import System.Directory
import System.Exit
import System.IO

import Data.Function
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Word

import Control.Monad
import Control.Applicative

prompt :: IO String
prompt = liftM (++ " $ ") getCurrentDirectory 

main = forever ( (prompt >>= putStr) *> hFlush stdout *> (getLine >>= runLine)) 

--main = do
--  (pid1,out,_) <- exec "/usr/bin/ls"   [   ] True  False Nothing
--  (pid2,_,_)   <- exec "/usr/bin/grep" ["c"] False False ( out )
--  void . wait $ pid1
--  void . wait $ pid2

runLine :: String -> IO ()
runLine [] = return ()
runLine "exit" = putStrLn "exit" *> exitSuccess
runLine l = do
  f <- withPath (head . words $ l) 
  let as = tail . words $ l :: [String]
  if null f then putStrLn "not found" else do
    (pid,_,_) <- exec (fromJust f) as False False Nothing
    void . wait $ pid

withPath :: String -> IO (Maybe String)
withPath s = liftM2 (on (++) (':':))  getCurrentDirectory (fmap concat $ lookupEnv "PATH") >>= filterM doesFileExist . map (++ "/" ++ s) . (splitOn ":") >>= return . listToMaybe
