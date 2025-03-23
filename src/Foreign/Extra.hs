{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Foreign.Extra where

import Foreign
import Foreign.C.Types

zeroMemory :: Ptr a -> Int -> IO ()
zeroMemory ptr size = do
  let charPtr = castPtr ptr :: Ptr CChar
  pokeArray charPtr (replicate size 0)

isZeroMemory :: Ptr a -> Int -> IO Bool
isZeroMemory ptr numBytes = do
  let bytePtr = castPtr ptr :: Ptr Word8
  bytes <- traverse (peek . plusPtr bytePtr) [0..numBytes - 1] :: IO [Word8]
  pure $ all (== 0) bytes

peekMaybe :: forall a. (Storable a) => Ptr a -> IO (Maybe a)
peekMaybe ptr = do
  let size = sizeOf (undefined :: a)
  isZero <- isZeroMemory ptr size
  if isZero then
    pure Nothing
  else
    Just <$> peek ptr