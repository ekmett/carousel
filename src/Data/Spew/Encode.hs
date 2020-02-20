{-# Language BlockArguments #-}
module Data.Spew.Encode
  ( encode
  -- internals
  , Chunk(..)
  , stitch
  , carve
  , encodePayload
  , spew
  ) where

import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Foldable as Foldable
import Data.Serialize
import Data.Spew.Codec
import Data.Spew.Hashed
import Data.Spew.Packet
import Data.Spew.Util
import Data.Vector as V
import Data.Vector.Split
import Data.Vector.Storable.ByteString
import Data.Word
import Galois.Matrix as Mat
import HaskellWorks.Data.ByteString (resegmentPadded)
import Network.Socket
import System.IO

-- a @dataShards codec x PAYLOAD_SIZE@ matrix
newtype Chunk = Chunk
  { chunkMatrix :: Matrix
  }
    
-- assumes input is exactly @dataShards codec * PAYLOAD_SIZE@ in size
carve :: Strict.ByteString -> Chunk
carve = Chunk . Matrix PAYLOAD_SIZE . V.fromList . chunksOf PAYLOAD_SIZE . byteStringToVector

-- concatenate the bytestrings yourself to get the original
stitch :: Chunk -> [Strict.ByteString]
stitch = fmap vectorToByteString . V.toList . vec . chunkMatrix
   
-- TODO: seed a pseudo-random shuffle based on shardId for periodic noise handling
shuffleWith :: Word8 -> Int -> [Int]
shuffleWith _ i = [0..i-1]

encodePayload :: Codec -> Word8 -> Chunk -> Strict.ByteString
encodePayload codec shardId chunk = 
  vectorToByteString $
    if s < dataShards codec
    then row cd s
    else row (rowmatrix (codecMatrix codec) (fromIntegral shardId) <> cd) 0
  where
    s = fromIntegral shardId 
    cd = chunkMatrix chunk

-- eventually do something other than ^C to quit here
spew :: Codec -> SockAddr -> Strict.ByteString -> Lazy.ByteString -> IO ()
spew codec sockaddr fileName content = do
  sock <- socket AF_INET Datagram 0
  setSocketOption sock Broadcast 1
  connect sock sockaddr
  handle <- socketToHandle sock WriteMode
  let packets = encodePackets codec fileName content
  Foldable.forM_ packets $ Strict.hPutStr handle . runPut . put . Hashed

encodePackets :: Codec -> Strict.ByteString -> Lazy.ByteString -> [Packet]
encodePackets codec fileName content =
  [ Packet
     (encodePayload codec shardId $ chunks V.! chunkId)
     fileSize
     (fromIntegral chunkId)
     shardId
     (fromIntegral $ dataShards codec)
     fileName
  | shardId <- [0..fromIntegral $ shards codec - 1]
  , chunkId <- shuffleWith shardId $ fromIntegral numChunks
  ] where

  fileSize :: Word64
  fileSize = fromIntegral $ Lazy.length content

  cs :: Word64 -- size of a chunk <= 255k
  cs = PAYLOAD_SIZE * fromIntegral (dataShards codec)

  numChunks :: Word64
  numChunks = cdiv fileSize cs

  chunks = V.fromList $
    carve <$> resegmentPadded (fromIntegral cs) (Lazy.toChunks content)
