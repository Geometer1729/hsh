{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SysCall where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Array

import System.Environment

import Control.Monad

import Data.Word

import GHC.IO.Handle
import System.Posix.Types
import System.Posix.IO

--foreign import capi "c/exec.c exec" cexec :: CString -> Ptr CString -> Ptr CString -> IO Int
foreign import capi "c/dup.c cexec" cexec :: CString -> Ptr CString -> Ptr CString -> IO (Ptr Word32)
foreign import capi "c/wait.c h_wait" cwait :: Int -> IO Int
foreign import capi "stdlib.h free" freec :: Ptr a -> IO ()


cifyListStr :: [String] -> IO (Ptr CString)
cifyListStr ss = (sequence . map newCString . maybeNullByte $ ss) >>= newArray 

maybeNullByte :: [String] -> [String]
maybeNullByte xs = if length xs > 2 then xs ++ ["\0"] else xs

wait :: Int -> IO ()
wait = void . cwait

exec :: String -> [String] -> IO (Int,Handle)
exec filePath args = do
    ptr <- join $ liftM3 cexec (newCString filePath) (cifyListStr $ "":args) (getEnvironment >>= cifyListStr . formatEnv)
    [pid,fd] <- (peekArray 2 ptr)
    void $ freec ptr
    h <- fdToHandle (fromIntegral fd)
    return (fromIntegral pid,h)


formatEnv :: [(String,String)] -> [String]
formatEnv xs = [ a ++ "=" ++ b | (a,b) <- xs]

