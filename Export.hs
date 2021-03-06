{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
module Export where

import Control.Monad.State
import System.IO
import Types
import qualified Data.Map as M

class Exportable a where
  export :: a -> [String] -> IO (Maybe String)

instance Exportable String where
  export s [] = return (Just s)
  export _ _  = return Nothing

instance Exportable (IO String) where
  export s [] = fmap Just s
  export _ _ = return Nothing

instance Show a => Exportable a where
  export x [] = return . Just . show $ x
  export _ _  = return Nothing

instance Exportable b => Exportable (String -> b) where
  export f (x:xs) = export (f x) xs
  export _ _ = return Nothing

instance (Exportable b , Read a) => Exportable (a -> b) where
  export f (x:xs) = export (f (read x)) xs
  export _ _ = return Nothing

type Table = M.Map String ([String] -> IO (Maybe String))

exportAs :: Exportable a => String -> a -> State Table ()
exportAs name x = modify $ M.insert name (export x)

tryTable :: Table -> [String] -> Maybe (IO (Maybe String))
tryTable t (x:xs) = case M.lookup x t of
  Nothing -> Nothing
  Just f -> Just $ f xs 
tryTable _ [] = Nothing

findExport :: [String] -> Maybe (IO (Maybe String))
findExport = tryTable exports

tryExport :: [String] -> Context -> Maybe (IO (CmdReturn))
tryExport args context = let mex = findExport args 
  in case mex of
    Nothing -> Nothing
    Just ex -> Just $ do
      mres <- ex
      case mres of
        Nothing  -> printTo (stout context) "wrong number of Args" >> return defRet{succes=False}
        Just res -> printTo (stout context) res                    >> return defRet

printTo :: Maybe Handle -> String -> IO ()
printTo Nothing  s = putStrLn  s
printTo (Just h) s = hPutStrLn h s

names :: [String]
names = M.keys exports

exports :: Table 
exports = flip execState (M.empty) $ do
  exportAs "(++)" ((++) :: String -> String -> String ) 
  exportAs "(+)"  ((+)  :: Double -> Double -> Double)
  exportAs "(-)"  ((-)  :: Double -> Double -> Double)
  exportAs "(*)"  ((*)  :: Double -> Double -> Double)
  exportAs "(/)"  ((/)  :: Double -> Double -> Double)
  exportAs "getLine" getLine
  exportAs "readFile" readFile

