{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CircularBuffer (
    CircularBuffer(..)
  , create
  , put
  , put'
  , get
  ) where

import           Data.Void (Void)

import           Foreign.C.Types (CUChar (..), CInt (..), CSize (..))
import           Foreign.ForeignPtr ( withForeignPtr, mallocForeignPtr, ForeignPtr, newForeignPtr )
import           Foreign.Ptr ( Ptr, FunPtr )
import           Foreign.Storable ( Storable(..) )
import           GHC.Generics (Generic)

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)


-- | The CircularBuffer constructor
newtype CircularBuffer =
  CircularBuffer (ForeignPtr Void)
  deriving (Eq, Ord, Show, Generic)


create :: CSize -> IO CircularBuffer
create sze = do
  ptr <- createInternal sze >>= newForeignPtr freeInternal
  return (CircularBuffer ptr)


get :: CircularBuffer -> IO (Maybe CUChar)
get (CircularBuffer cb) = do
  fpout <- mallocForeignPtr
  ok <-
    withForeignPtr cb $ \cbp -> do
      withForeignPtr fpout $ \pout -> do
        getInternal cbp pout

  if ok == 0 then Just <$> (withForeignPtr fpout peek) else return Nothing


put :: CircularBuffer -> CUChar -> IO ()
put (CircularBuffer cb) v =
  withForeignPtr cb $ \cbp ->
    putInternal cbp v


-- | Put, but only if there's space
--   True on successful put, False on full
put' :: CircularBuffer -> CUChar -> IO Bool
put' (CircularBuffer cb) v = do
  ok <-
    withForeignPtr cb $ \cbp ->
      putInternal2 cbp v
  return $ ok == 0


foreign import ccall unsafe "circular_buf_init"
  createInternal :: CSize -> IO (Ptr Void)

foreign import ccall unsafe "&circular_buf_free"
  freeInternal :: FunPtr (Ptr Void -> IO ())

foreign import ccall unsafe "circular_buf_reset"
  resetInternal :: Ptr Void -> IO ()

foreign import ccall unsafe "circular_buf_put"
  putInternal :: Ptr Void -> CUChar -> IO ()

foreign import ccall unsafe "circular_buf_put2"
  putInternal2 :: Ptr Void -> CUChar -> IO CInt

foreign import ccall unsafe "circular_buf_get"
  getInternal :: Ptr Void -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "circular_buf_empty"
  emptyInternal :: Ptr Void -> IO ()

foreign import ccall unsafe "circular_buf_full"
  fullInternal :: Ptr Void -> IO ()

foreign import ccall unsafe "circular_buf_capacity"
  capacityInternal :: Ptr Void -> IO ()

foreign import ccall unsafe "circular_buf_size"
  sizeInternal :: Ptr Void -> IO ()
