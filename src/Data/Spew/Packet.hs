{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}

module Data.Spew.Packet
  ( Packet(..)
  ) where

import Data.ByteString
import Data.Serialize
import Data.Word

-- filename.xxx.nnn.spew, metadata gets written at the end of spewdata
-- xxx = # of datashards
-- nnn = filesize in bytes
-- at the end after we do an in place fix we can truncate
--
-- filelen meta = cdiv filesize (codecPayloadSize * dataShards) * dataShards bytes
-- each one contains a digit for if we've loaded that chunk and digit the hash
--
-- round up file size to 64k and divide by 1k
-- this many bytes

data Packet = Packet
  { packetFileSize    :: {-# UNPACK #-} !Word64
  , packetChunkId     :: {-# UNPACK #-} !Word64
  , packetShardId     :: {-# UNPACK #-} !Word8
  , packetDataShards  :: {-# UNPACK #-} !Word8 -- determine data file and codec
  , packetPayload     :: {-# UNPACK #-} !ByteString -- exactly codecPayload bytes
  , packetFileName    :: !ByteString
  } -- deriving Show

instance Show Packet where
  showsPrec d Packet{..} = showParen (d > 10) $
    showString "Packet " . showsPrec 11 packetFileSize 
          . showChar ' ' . showsPrec 11 packetChunkId
          . showChar ' ' . showsPrec 11 packetShardId
          . showChar ' ' . showsPrec 11 packetDataShards
          . showChar ' ' . showsPrec 11 (fst $ spanEnd (==0) packetPayload) -- for sanity
          . showChar ' ' . showsPrec 11 packetFileName

putBS16 :: Putter ByteString
putBS16 = putNested (putWord16be . fromIntegral) . putByteString

getBS16 :: Get ByteString
getBS16 = do
  size <- fromIntegral <$> getWord16be
  getByteString size

instance Serialize Packet where
  put Packet{..} = do
    putWord64be packetFileSize
    putWord64be packetChunkId
    putWord8 packetShardId
    putWord8 packetDataShards
    putBS16 packetPayload
    putBS16 packetFileName
  get = do
    packetFileSize <- getWord64be
    packetChunkId <- getWord64be
    packetShardId <- getWord8
    packetDataShards <- getWord8
    packetPayload <- getBS16
    packetFileName <- getBS16
    pure Packet{..}
