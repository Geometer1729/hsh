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

import Data.Word

foreign import capi "ret2.c ret2c" ret2c :: IO (Ptr Word32)
foreign import capi "stdlib.h free" freec :: Ptr a -> IO ()

ret2 :: IO (Int,Int)
ret2 = do
  ptr <- ret2c 
  [x,y] <- peekArray 2 ptr 
  void $ freec ptr 
  return (fromIntegral x,fromIntegral y)

main = (sequence $ take 100 (cycle [ret2])) >>= print . filter (\(x,y) -> not $ and [ x == 5 , y == 7 ])
