{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}

module Data.Spew.Packet
  ( Packet(..)
  ) where

import Data.ByteString
import Data.Serialize

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
  { fileSize :: {-# UNPACK #-} !Int
  , chunk    :: {-# UNPACK #-} !Int
  , shard    :: {-# UNPACK #-} !Int
  , shards   :: {-# UNPACK #-} !Int
  , payload  :: {-# UNPACK #-} !ByteString
  , fileName :: !ByteString
  } -- deriving Show

instance Show Packet where
  showsPrec d Packet{..} = showParen (d > 10) $
    showString "Packet " . showsPrec 11 fileSize
          . showChar ' ' . showsPrec 11 chunk
          . showChar ' ' . showsPrec 11 shard
          . showChar ' ' . showsPrec 11 shards
          . showChar ' ' . showsPrec 11 (fst $ spanEnd (==0) payload) -- for sanity, we don't Read
          . showChar ' ' . showsPrec 11 fileName

putBS16 :: Putter ByteString
putBS16 = putNested (putWord16be . fromIntegral) . putByteString

getBS16 :: Get ByteString
getBS16 = do
  size <- fromIntegral <$> getWord16be
  getByteString size

instance Serialize Packet where
  put Packet{..} = do
    putWord64be $ fromIntegral fileSize
    putWord64be $ fromIntegral chunk
    putWord8 $ fromIntegral shard
    putWord8 $ fromIntegral shards
    putBS16 payload
    putBS16 fileName
  get = do
    fileSize <- fromIntegral <$> getWord64be
    chunk <- fromIntegral <$> getWord64be
    shard <- fromIntegral <$> getWord8
    shards <- fromIntegral <$> getWord8
    payload <- getBS16
    fileName <- getBS16
    pure Packet{..}
