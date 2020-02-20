{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}

module Data.Spew.Packet
  ( Packet(..)
  ) where

import Data.ByteString
import Data.Serialize

data Packet = Packet
  { fileSize :: {-# UNPACK #-} !Int
  , chunk    :: {-# UNPACK #-} !Int
  , shard    :: {-# UNPACK #-} !Int
  , shards   :: {-# UNPACK #-} !Int
  , payload  :: {-# UNPACK #-} !ByteString
  , fileName :: !ByteString
  }

instance Show Packet where
  showsPrec d Packet{..} = showParen (d > 10) $
    showString "Packet " . showsPrec 11 fileSize
          . showChar ' ' . showsPrec 11 chunk
          . showChar ' ' . showsPrec 11 shard
          . showChar ' ' . showsPrec 11 shards
          . showChar ' ' . showsPrec 11 (fst $ spanEnd (==0) payload) -- for readability
          . showChar ' ' . showsPrec 11 fileName

instance Serialize Packet where
  put Packet{..} = do
    putWord64be $ fromIntegral fileSize
    putWord64be $ fromIntegral chunk
    putWord8 $ fromIntegral shard
    putWord8 $ fromIntegral shards
    let putBS16 = putNested (putWord16be . fromIntegral) . putByteString
    putBS16 payload
    putBS16 fileName
  get = do
    fileSize <- fromIntegral <$> getWord64be
    chunk <- fromIntegral <$> getWord64be
    shard <- fromIntegral <$> getWord8
    shards <- fromIntegral <$> getWord8
    let getBS16 = getWord16be >>= getByteString . fromIntegral
    payload <- getBS16
    fileName <- getBS16
    pure Packet{..}
