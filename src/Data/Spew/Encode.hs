{-# Language BlockArguments #-}
module Data.Spew.Encode
  ( encode
  -- internals
  , lazyByteStringToVector
  , Chunk(..)
  , stitch
  , carve
  , encodePacket
  ) where

import Data.ByteString.Lazy as Lazy
import Data.Spew.Codec
import Data.Vector as V
import Data.Vector.Storable as S
import Data.Vector.Storable.ByteString
import Data.Word
import Galois.Field
import Galois.Matrix as Mat

-- |
-- @packet c i d@ assumes
--
-- @
-- rows d = dataShards c
-- cols d < 1024ish to fit into a nice packet mtu w/ padding and protocol overhead
-- 0 <= i < shards c
-- @
--
-- assuming a minimal valid mtu of 1280 for valid ipv6, 40 bytes ipv6 header, 8 bytes udp, 43 bytes chunk id, 128 bytes filename = 1061 bytes, so 1024 bytes gives some wiggle room for more protocol stuff as we find we need it.
--
-- packet = <8 byte chunk id, 1 byte shard id, 2 byte size, content, sha256> filename
encodePacket :: Codec -> Word64 -> Word8 -> Chunk -> Packet
encodePacket codec chunkId shardId chunk = Packet chunkId shardId (fromIntegral $ chunkSize chunk) $ vectorToByteString $
  if s < dataShards codec
  then row cd s
  else row (rowmatrix (vm codec) (fromIntegral shardId) <> cd) 0
  where
    s = fromIntegral shardId 
    cd = chunkData chunk

data Chunk = Chunk
  { chunkSize :: {-# UNPACK #-} !Int
  , chunkData :: {-# UNPACK #-} !Matrix
  }
    
carve :: Codec -> S.Vector G -> Chunk
carve codec v = Chunk (S.length v) $ Matrix bpp $ V.generate ds \i -> pad $ S.take bpp $ S.drop (bpp*i) v
  where
    ds = dataShards codec
    bpp = div (S.length v + ds - 1) ds -- round up number of bytes per chunk
    pad xs
      | n <- bpp - S.length xs, n > 0 = xs S.++ S.replicate n (G 0) -- ugh, TODO: modify matrices to allow ragged right margin
      | otherwise = xs

lazyByteStringToVector :: Lazy.ByteString -> S.Vector G
lazyByteStringToVector = byteStringToVector . Lazy.toStrict

-- lazyByteStringToVector . stitch c . carve c = id
stitch :: Codec -> Chunk -> Lazy.ByteString
stitch codec (Chunk n m) = Lazy.fromChunks
  [ vectorToByteString $ S.take (n - i * cols m) $ row m i
  | i <- [0..dataShards codec - 1]
  ]
  
  -- r /= 0 implies the first r rows get are size q+1, remainder are size q, and need 0 padding
  -- r == 0 implies the all rows are size q
  -- break d into @dataShards codec@ columns, padding any fractional ones

-- builds up a bunch of packets to spew over the network
encode :: Codec -> Lazy.ByteString -> [Packet]
encode codec lbs =
  [ encodePacket codec chunkId shardId $ chunks V.! fromIntegral chunkId
  | shardId <- [0..fromIntegral $ shards codec - 1]
  , chunkId <- [0..numChunks - 1]
  ] where
  numChunks = fromIntegral $ div (Lazy.length lbs + fromIntegral MAX_PACKET_DATA_SIZE - 1) (fromIntegral MAX_PACKET_DATA_SIZE)
  cs = fromIntegral MAX_PACKET_DATA_SIZE * fromIntegral (dataShards codec)
  chunks = V.generate (fromIntegral numChunks) \i ->
    carve codec $ lazyByteStringToVector $ Lazy.take (fromIntegral cs) $ Lazy.drop (cs * fromIntegral i) lbs
