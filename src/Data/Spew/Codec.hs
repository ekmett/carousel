{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}

module Data.Spew.Codec
  ( Codec(..)
  , new
  , pattern SHARDS
  , parityShards
  ) where

import Data.Default
import Galois.Matrix as Mat
import Data.Vector as V
import Data.Vector.Storable as S

-- @SHARDS x dataShards c@ matrix represented by a storable vector
type CodecMatrix = Vec

data Codec = Codec
  { payloadSize :: {-# UNPACK #-} !Int
  , dataShards  :: {-# UNPACK #-} !Int
  -- , codecMatrix :: {-# UNPACK #-} !Matrix
  , packedCodec :: {-# UNPACK #-} !CodecMatrix
  } deriving Show

pattern SHARDS :: Int
pattern SHARDS = 255

parityShards :: Codec -> Int
parityShards c = SHARDS - dataShards c

new :: Int -> Int -> Codec
new pls ds 
  | ds <= 0   = error "no data shards"
  | ds >= 256 = error "too many data shards"
  | pls == 0  = error "no payload"
  | pls > 65535 = error "payload too large"
  | otherwise = Codec pls ds pm
  where
    ss = 255
    vm = vandermonde ss ds
    m = Mat.mul vm $ Mat.inv $ submatrix vm 0 0 ds ds
    pm = S.concat $ V.toList (vec m)

instance Default Codec where
  def = new 1024 16
