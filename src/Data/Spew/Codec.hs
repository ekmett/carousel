{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}
{-# Language ImplicitParams #-}
{-# Language ConstraintKinds #-}
{-# Language RankNTypes #-}

module Data.Spew.Codec
  ( Codec(..)
  , GivenCodec
  , new
  , with
  , pattern SHARDS
  , parityShards
  , dataShards
  , payloadSize
  , code
  ) where

import Data.Default
import Galois.Matrix as Mat
import Data.Vector as V
import Data.Vector.Storable as S

-- @SHARDS x dataShards c@ matrix represented by a storable vector
type CodecMatrix = Vec

data Codec = Codec
  { _payloadSize :: {-# UNPACK #-} !Int
  , _dataShards :: {-# UNPACK #-} !Int
  , _code :: {-# UNPACK #-} !CodecMatrix
  }


payloadSize :: GivenCodec => Int
payloadSize = _payloadSize ?codec

dataShards :: GivenCodec => Int
dataShards = _dataShards ?codec

code :: GivenCodec => CodecMatrix
code = _code ?codec

type GivenCodec = (?codec :: Codec)

pattern SHARDS :: Int
pattern SHARDS = 255

parityShards :: GivenCodec => Int
parityShards = SHARDS - dataShards

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

with :: Codec -> (GivenCodec => r) -> r
with c f = let ?codec = c in f

instance Default Codec where
  def = new 1024 16
