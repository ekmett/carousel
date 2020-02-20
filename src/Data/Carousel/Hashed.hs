module Data.Carousel.Hashed
  ( Hashed(..)
  ) where

import Data.ByteString
import Data.Serialize
import Control.Monad (when)
import Control.Monad.Fail as Monad
import Crypto.Hash.SHA256 as Hash

sha256 :: ByteString -> ByteString
sha256 bs = Hash.finalize $ Hash.update Hash.init bs

newtype Hashed a = Hashed { getHashed :: a }

instance Serialize a => Serialize (Hashed a) where
  put (Hashed a) = do 
    let bs = runPut (put a)
    putByteString bs
    putByteString $ sha256 bs
  get = do
    a <- get
    let bs = runPut (put a)
    receivedHash <- getByteString 32
    when (receivedHash /= sha256 bs) $ Monad.fail "hash mismatch"
    pure $ Hashed a
