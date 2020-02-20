{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}

module Data.Spew.Packet
  ( Packet(..)
  , pattern PAYLOAD_SIZE
  ) where

import Data.ByteString
import Data.Serialize
import Data.Word

pattern PAYLOAD_SIZE :: (Eq a, Num a) => a
pattern PAYLOAD_SIZE = 1024

-- filename.xxx.nnn.spew, metadata gets written at the end of spewdata
-- xxx = # of datashards
-- nnn = filesize in bytes
-- at the end after we do an in place fix we can truncate
--
-- filelen meta = cdiv filesize (PAYLOAD_SIZE * dataShards) * dataShards bytes
-- each one contains a digit for if we've loaded that chunk and digit the hash
--
-- round up file size to 64k and divide by 1k
-- this many bytes

data Packet = Packet
  { packetPayload    :: {-# UNPACK #-} !ByteString -- exactly PAYLOAD_SIZE bytes
  , packetFileSize   :: {-# UNPACK #-} !Word64
  , packetChunkId    :: {-# UNPACK #-} !Word64
  , packetShardId    :: {-# UNPACK #-} !Word8
  , packetDataShards :: {-# UNPACK #-} !Word8 -- determine data file and codec
  , packetFileName   :: !ByteString
  } deriving Show

instance Serialize Packet where
  put Packet{..} = do
    putByteString packetPayload
    putWord64be packetFileSize
    putWord64be packetChunkId
    putWord8 packetShardId
    putWord8 packetDataShards
    putNested (putWord8 . fromIntegral) $ putByteString packetFileName
  get = do
    packetPayload <- getByteString PAYLOAD_SIZE
    packetFileSize <- getWord64be
    packetChunkId <- getWord64be
    packetShardId <- getWord8
    packetDataShards <- getWord8
    packetFileNameSize <- fromIntegral <$> getWord8
    packetFileName <- getByteString packetFileNameSize
    pure Packet{..}
