module SubUtils where


import System.Environment

-- sub utils

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

dosubs :: [(String,String)] -> String -> String
dosubs [] s = s
dosubs ((m,r):xs) s = dosubs xs (sub m r s)

