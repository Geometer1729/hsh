{-# LANGUAGE LambdaCase #-}
module Exec where

import Data.Default
import System.Directory
import System.Environment
import System.IO
import System.Posix.Directory
import System.Process
import System.Exit
import Types
import Control.Monad
import Parse
import SubUtils
import Data.Char
import ScriptUtil
import Data.Maybe
import Export

import {-# Source #-} CmdHandle

type Attempt = String -> [String] -> Context -> IO (Maybe (IO CmdReturn))

seqAttempts :: [Attempt] -> Attempt
seqAttempts (x:xs) a b c = do
  r1 <- x a b c
  case r1 of
    Just ret -> return . Just $ ret
    Nothing -> seqAttempts xs a b c
seqAttempts [] _ _ _ = return $ Nothing

doExec :: String -> [String] -> Context -> IO CmdReturn
doExec name args context = do
  (rets,args') <- fmap unzip $ mapM expandArg args
  let ret = mconcat rets
  if succes ret 
    then do
      r <- findExec name (concat args') context
      case r of
        Just c -> do
          retc <- c
          return $ ret <> retc
        Nothing -> putStrLn ("no such command could be found " ++ name) >> return def{succes=False}
    else return $ ret

expandArg :: String -> IO (CmdReturn,[String])
expandArg w@('`' :xs) = if last xs == '`'  then do
  let ml = parseLine $ init xs
  case ml of
    Nothing -> return (def{succes=False},[]) -- this needs to be better
    Just l -> eval l
  else return (def,[w])
expandArg w@('"' :xs) = fmap ((,) def) $ if last xs == '"'  then return [init xs] else return [w]
expandArg w@('\'':xs) = fmap ((,) def) $ if last xs == '\'' then return [init xs] else return [w]
expandArg arg = fmap ((,) def) $ (varExpand >=> deTildify >=> glob) arg

findExec :: Attempt
findExec = seqAttempts [tryVar,tryLambda,tryBuiltin,tryExec,tryHask]

varExpand :: String -> IO String
varExpand ('$':w) = let
  (var,rest) = break (not . isAlpha) w
  in do
    mval <- lookupEnv var
    case mval of
      Just val -> fmap (val ++) $ varExpand rest
      Nothing -> varExpand rest
varExpand (c:w) = fmap (c:) $ varExpand w
varExpand [] = return []

tryVar :: String -> [String] -> Context -> IO (Maybe (IO CmdReturn))
tryVar path args context = lookupEnv path >>= \case
  Just string  -> do
    return . Just $ contextHandleLine context (string ++ " " ++ unwords args) 
  Nothing -> return Nothing

tryLambda :: String -> [String] -> Context -> IO (Maybe (IO CmdReturn))
tryLambda ('(':'\\':string) args context = 
  if last string /= ')' 
    then return Nothing
    else do
      let string' = init string
      let ws = words string'
      let vars = takeWhile (/= "->") ws
      let functionOutput = unwords . tail . dropWhile (/= "->") $ ws
      case compare args vars of
            GT -> return Nothing
            EQ -> do
              let subs = zip vars args 
              let output = doSubs subs functionOutput
              return . Just $ contextHandleLine context output
            LT -> let
              flowableBools = map (\x -> last x == 's') vars
              flowTarget = listToMaybe $ map fst . filter snd $ (zip [0..] flowableBools)
              in case flowTarget of
                Nothing -> do
                  let output = doSubs (zip vars args) functionOutput ++ " " ++ unwords (drop (length vars) args)
                  return . Just $ contextHandleLine context output
                Just n -> do  
                  let extra = length args - length vars
                  let subList = take (n) args ++ [unwords . take (extra + 1) . drop  n $args] ++ (drop (n+extra+1) args)
                  let subs = zip vars subList
                  let output = doSubs subs functionOutput
                  return . Just $ contextHandleLine context output
tryLambda _ _ _ = return Nothing


tryBuiltin :: String -> [String] -> Context -> IO (Maybe (IO CmdReturn))
tryBuiltin cmd rawArgs context = case (cmd,rawArgs) of
    ("exit",_)       -> return . Just $ return def{shellExit=True}
    ("cd",args)      -> return . Just $ fromSuc $ cd args 
    ("print",args)   -> return . Just $ fromSuc $ printEnvVars args 
    ("lineMap",args) -> return . Just $ lineMap args context
    (".",args)       -> return . Just $ runFiles args
    ("True",[])      -> return . Just $ true
    ("False",[])     -> return . Just $ false
    _                -> return $ Nothing

fromSuc :: IO Bool -> IO CmdReturn
fromSuc = fmap (\s -> def{succes=s})


tryExec :: String -> [String] -> Context -> IO (Maybe (IO CmdReturn))
tryExec path args context = do
  localEnv <- getEnvironment
  fromPath <- findExecutables path
  localCwd <- getCurrentDirectory
  path' <- deTildify path
  fromLocal <- findExecutablesInDirectories ["",localCwd] path'
  let execName = listToMaybe (fromPath ++ fromLocal)
  case execName of
    Just location -> do
      procHandle <- runProcess location args Nothing (Just localEnv) (stin context) (stout context) (sterr context)
      -- if not awaiting the return code should be ignored
      case wait context of
        Do -> do
          suc <- fmap (== ExitSuccess) $ waitForProcess procHandle 
          return . Just . return $ CmdReturn False suc [] 
        Dont -> do
          return . Just . return $ CmdReturn False True [] 
        PassHandles -> do
          return . Just . return $ CmdReturn False True [procHandle] 
    Nothing -> return Nothing

tryHask :: String -> [String] -> Context -> IO (Maybe (IO CmdReturn))
tryHask func args context = return $ tryExport (func:args) context

eval :: Line -> IO (CmdReturn,[String])
eval w = do
  (readEnd,writeEnd) <- createPipe
  ret <- contextHandleLineData def{stout = Just writeEnd,wait = PassHandles} w
  out <- hGetContents readEnd 
  ret' <- withWaits ret
  return (ret',concat . map words . lines $ out)

withWaits :: CmdReturn -> IO CmdReturn 
withWaits ret = do
  s <- waitFor (awaits ret)
  return $ ret <> def{succes=s}

waitFor :: [ProcessHandle] -> IO Bool
waitFor ps = fmap and $ mapM waitOne ps

waitOne :: ProcessHandle -> IO Bool
waitOne p = fmap (== ExitSuccess) $ waitForProcess p

-- Builtins

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
cd (_:_:_) = putStrLn "too many args to cd" >> return False

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
      [] -> error "let func called on []"
      [_] -> unwords right
      _ -> "(\\" ++ (unwords . tail $ left) ++ " -> " ++ unwords right ++ ")"
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
lineMap []      _       = lineMapArgFail
lineMap (_:[])  _       = lineMapArgFail
lineMap (f:cmd) context = do
  (readEnd,writeEnd) <- createPipe
  cmdRet <- contextHandleLine context{stout = Just writeEnd,wait=PassHandles} (unwords cmd)
  lineRets <- lineMapLoop f readEnd context
  awaitSuc <-  mapM waitForProcess (awaits cmdRet)
  let cmdRet' = cmdRet <> def{succes=and . map  (== ExitSuccess) $ awaitSuc}
  return $ mconcat (cmdRet':lineRets)

lineMapArgFail :: IO CmdReturn
lineMapArgFail = do
  putStrLn "lineMap requires atleast 2 arguments"
  return $ def{succes=False}

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
