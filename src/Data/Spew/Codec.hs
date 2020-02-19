{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}

module Data.Spew.Codec
  ( Codec(..)
  , new
  , shards
  , pattern MAX_PACKET_DATA_SIZE
  , Packet(..)
  ) where

import Control.Monad (when)
import Control.Monad.Fail as Monad
import Crypto.Hash.SHA256 as Hash
import Data.ByteString
import Data.Default
import Data.Serialize
import Data.Word
import Galois.Matrix as Mat

-- Data shards = 64
-- Parity shards = 191

data Codec = Codec
  { dataShards   :: {-# UNPACK #-} !Int
  , parityShards :: {-# UNPACK #-} !Int
  , vm           :: {-# UNPACK #-} !Matrix -- vandermonde (ds+ps) ds <> inv (vandermonde ds ds)
  } deriving Show

shards :: Codec -> Int
shards c = dataShards c + parityShards c

-- new 64 191
new :: Int -> Int -> Codec
new ds ps
  | ds <= 0   = error "no data shards"
  | ps < 0    = error "negative parity shards"
  | ss >= 256 = error ">= 256 total shards"
  | otherwise = Codec ds ps $ Mat.mul vm $ Mat.inv $ submatrix vm 0 0 ds ds
  where
    ss = ds + ps
    vm = vandermonde ss ds

instance Default Codec where
  def = new 64 191 -- 255 shards per chunk, 64 of which are original data @ 1k each

pattern MAX_PACKET_DATA_SIZE :: Int
pattern MAX_PACKET_DATA_SIZE = 1024

data Packet = Packet
  { packetChunkId  :: {-# UNPACK #-} !Word64
  , packetShardId  :: {-# UNPACK #-} !Word8
  , packetDataSize :: {-# UNPACK #-} !Word32
  , packetData     :: {-# UNPACK #-} !ByteString
  }

unhashedPayload :: Packet -> ByteString
unhashedPayload Packet{..} = runPut do
  putWord64be packetChunkId
  putWord8 packetShardId
  putWord32be packetDataSize -- 0 means 64*1024
  putNested (putWord16be . fromIntegral) (putByteString packetData)

sha256 :: ByteString -> ByteString
sha256 bs = Hash.finalize $ Hash.update Hash.init bs

instance Serialize Packet where
  put p = do
    let uhp = unhashedPayload p
    putByteString uhp
    putByteString $ sha256 uhp
  get = do
    packetChunkId <- getWord64be
    packetShardId <- getWord8
    packetDataSize <- getWord32be
    packetData <- getByteString (fromIntegral packetDataSize)
    let p = Packet{..}
    receivedHash <- getByteString 32 
    when (receivedHash /= sha256 (unhashedPayload p)) $ Monad.fail "bad hash"
    pure p
