{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}

module Data.Spew.Codec
  ( Codec(..)
  , new
  , shards
  , dataShards
  , parityShards
  ) where

import Data.Default
import Galois.Matrix as Mat

newtype Codec = Codec
  { codecMatrix :: Matrix
  } deriving Show

shards :: Codec -> Int
shards = rows . codecMatrix

dataShards :: Codec -> Int
dataShards = cols . codecMatrix 

parityShards :: Codec -> Int
parityShards c = rows m - cols m where m = codecMatrix c

new :: Int -> Codec
new ds
  | ds <= 0   = error "no data shards"
  | ds >= 256 = error "too many data shards"
  | otherwise = Codec $ Mat.mul vm $ Mat.inv $ submatrix vm 0 0 ds ds
  where
    ss = 255
    vm = vandermonde ss ds

instance Default Codec where
  def = new 64
