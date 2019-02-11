{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}


import GHC.IO.Handle
import System.Posix.Types
import System.Posix.IO

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Array

import System.Environment

import Control.Monad

foreign import capi "pipe.c pipeC" pipeC :: CString -> IO Fd

pipeTo :: String -> IO Handle
pipeTo s = newCString s >>= pipeC >>= fdToHandle 

main = do
  h <- pipeTo "test string"
  text <- hGetContents h
  putStrLn text
  hClose h
