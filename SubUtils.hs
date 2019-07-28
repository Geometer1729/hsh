module SubUtils where


import System.Environment
import System.Directory
import Control.Monad

glob :: String -> IO [String]
glob path 
  | not $ '*' `elem` path = return [path]
  | head path == '/' = globalGlob path
  | otherwise        = localGlob path

localGlob :: String -> IO [String]
localGlob path = do
  pwd <- getCurrentDirectory
  globalGlob (pwd ++ "/" ++ path)

globalGlob :: String -> IO [String]
globalGlob path = let list = tail . splitSlash $ path in foldM (flip stepGlob) ["/"] list

stepGlob :: String -> [String] -> IO [String]
stepGlob pattern state = fmap concat $ mapM (flip expandStar pattern) state

expandStar :: String -> String -> IO [String]
expandStar dir pattern = do
  isDir <- doesDirectoryExist dir
  if isDir then do
    options <- listDirectory dir
    let valid = filter (starMatch pattern) options
    let dir' = if last dir == '/' then dir else dir ++ "/"
    return [ dir' ++ option | option <- valid ]
  else do
    return []

starMatch :: String -> String -> Bool
starMatch ('*':_) ('.':_) = False --prevents matching hidden files with * .* however would work
starMatch xs ys = simpleMatch xs ys
  
simpleMatch :: String -> String -> Bool
simpleMatch ('*':xs) (y:ys) = starMatch xs (y:ys) || starMatch ('*':xs) ys
simpleMatch (x:xs)   (y:ys) = (x == y) && (starMatch xs ys)
simpleMatch [] [] = True
simpleMatch "*" [] = True
simpleMatch _  _  = False

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
sub match replace input = if isPrefix match input then replace ++ (sub match replace (drop (length match) input)) else (head input) : (sub match replace (tail input))

dosubs :: [(String,String)] -> String -> String
dosubs [] s = s
dosubs ((m,r):xs) s = dosubs xs (sub m r s)

splitPath,splitSlash :: String -> [String]
splitPath = splitOn ':'
splitSlash = splitOn '/'

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = let (x,xs) = break (== c) s in case xs of
  "" -> [x]
  w -> x: (splitOn c . tail $ w)

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys
isPrefix [] _ = True
isPrefix _ [] = False

