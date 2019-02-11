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
foreign import capi "c/exec.c cexec" cexec :: CString -> Ptr CString -> Ptr CString -> Bool -> Bool -> IO (Ptr Word32)
foreign import capi "c/wait.c h_wait" cwait :: Int -> IO Int
foreign import capi "stdlib.h free" freec :: Ptr a -> IO ()

cifyListStr :: [String] -> IO (Ptr CString)
cifyListStr ss = (sequence . map newCString $ ss) >>= newArray0 nullPtr

wait :: Int -> IO ()
wait = void . cwait

exec :: String -> [String] -> Bool -> Bool -> IO (Int,Maybe Handle,Maybe Handle)
exec filePath args out err= do
    ptr <- join $ liftM5 cexec (newCString filePath) (cifyListStr $ "":args) (getEnvironment >>= cifyListStr . formatEnv) (return out) (return err)
    array <- (peekArray (1 + length (filter id [out,err])) ptr)
    void $ freec ptr
    let pid = fromIntegral . head $ array :: Int
    outH <- if out then fmap Just $ fdToHandle (fromIntegral $ array !! 1)                      else return Nothing
    errH <- if err then fmap Just $ fdToHandle (fromIntegral $ array !! (if out then 2 else 1)) else return Nothing
    return $ (pid,outH,errH)


formatEnv :: [(String,String)] -> [String]
formatEnv xs = [ a ++ "=" ++ b | (a,b) <- xs] 

