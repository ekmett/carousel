{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}
{-# Language ImplicitParams #-}
{-# Language ConstraintKinds #-}
{-# Language RankNTypes #-}

module Data.Carousel.Codec
  ( Codec(..)
  , GivenCodec
  , new
  , with
  , pattern SHARDS
  , parityShards
  , dataShards
  , payloadSize
  , code
  , shardRow
  , shardRows
  ) where

import Data.Default
import Galois.Field
import Galois.Matrix as Mat
import Data.Vector as V
import Data.Vector.Storable as S

-- packed @SHARDS x dataShards c@ matrix represented by a storable vector
type CodecMatrix = Vec

data Codec = Codec
  { _payloadSize :: {-# UNPACK #-} !Int
  , _dataShards :: {-# UNPACK #-} !Int
  , _code :: {-# UNPACK #-} !CodecMatrix
  } deriving Show

type GivenCodec = (?codec :: Codec)

payloadSize :: GivenCodec => Int
payloadSize = _payloadSize ?codec

dataShards :: GivenCodec => Int
dataShards = _dataShards ?codec

code :: GivenCodec => CodecMatrix
code = _code ?codec

-- shard row
shardRow :: GivenCodec => Int -> Vec
shardRow i = S.slice (dataShards*i) dataShards code

shardRows :: GivenCodec => [Int] -> Matrix
shardRows = Matrix dataShards . V.fromList . fmap shardRow 

pattern SHARDS :: Int
pattern SHARDS = 255

parityShards :: GivenCodec => Int
parityShards = SHARDS - dataShards

new :: Int -> Int -> Codec
new pls ds 
  | ds <= 0   = error "no data shards"
  | ds >= 256 = error "too many data shards"
  | pls <= 0  = error "no payload"
  | pls > 65535 = error "payload too large"
  | otherwise = Codec pls ds $ cache V.! (ds-1)

with :: Codec -> (GivenCodec => r) -> r
with c f = let ?codec = c in f

instance Default Codec where
  def = new 1024 16

cache :: V.Vector CodecMatrix
cache = V.generate 255 \i -> let
    ds = i + 1
    vm = vandermonde 255 ds
    m = Mat.mul vm $ Mat.inv $ submatrix vm 0 0 ds ds
  in S.concat $ V.toList (vec m)

vandermonde :: Int -> Int -> Matrix
vandermonde rs cs = Mat.generate rs cs \i j -> X i ^ j
