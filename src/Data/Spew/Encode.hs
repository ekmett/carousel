{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}
module Data.Spew.Encode
  ( encode
  -- internals
  , Chunk
  , encodePayload
  , spew
  ) where

import Control.Monad (when)
import Control.Loop (numLoop)
import Control.Monad.IO.Class
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Foldable as Foldable
import Data.Serialize
import Data.Spew.Codec
import Data.Spew.Hashed
import Data.Spew.Packet
import Data.Spew.Util
import Data.Vector as V
import Data.Vector.Storable as S
import Data.Vector.Storable.Mutable as MS
import Data.Vector.Storable.ByteString
import Galois.Field
import Galois.Matrix as Mat
import HaskellWorks.Data.ByteString
import Network.Socket
import Network.Socket.ByteString as Strict
import System.IO

-- an @dataShards x payloadSize@ row major matrix as a single vector
type Chunk = Vec
    
-- TODO: seed a pseudo-random shuffle based on shard for periodic noise handling
shuffleWith :: Int -> Int -> [Int]
shuffleWith _ i = [0..i-1]

encodePayload :: Codec -> Int -> Chunk -> Vec
encodePayload Codec{..} shard chunk
  | shard < dataShards = S.slice (payloadSize*shard) payloadSize chunk
  | otherwise = S.create do
  mv <- MS.new payloadSize
  let r = S.drop (dataShards*shard) packedCodec
  gvmul mv (S.head r) chunk
  mv <$ numLoop 1 (S.length r - 1) \i ->
    gvfma mv (r S.! i) $ S.drop (payloadSize*i) chunk

-- eventually do something other than ^C to quit here
spew :: MonadIO m => Codec -> SockAddr -> Bool -> 
   m (Strict.ByteString -> Lazy.ByteString -> IO ())
spew codec sockaddr broadcast = liftIO do
  System.IO.putStrLn "Opening socket"
  sock <- socket AF_INET Datagram 0
  when broadcast $ setSocketOption sock Broadcast 1
  connect sock sockaddr
  return \fileName content -> do
    System.IO.putStrLn "encoding packets"
    let packets = encodePackets codec fileName content
    print packets
    System.IO.putStrLn "sending packets"
    Foldable.forM_ packets $
      Strict.send sock . runPut . put . Hashed

encodePackets :: Codec -> Strict.ByteString -> Lazy.ByteString -> [Packet]
encodePackets codec@Codec{..} fileName content = do
  shardId <- [0..SHARDS-1]
  chunkId <- shuffleWith shardId numChunks
  pure Packet
    { packetPayload = vectorToByteString $ encodePayload codec shardId $ chunks V.! chunkId
    , packetFileSize = fromIntegral fileSize
    , packetChunkId = fromIntegral chunkId
    , packetShardId = fromIntegral shardId
    , packetDataShards = fromIntegral dataShards
    , packetFileName =  fileName
    }
  where
    fileSize = fromIntegral $ Lazy.length content
    chunkSize = payloadSize * dataShards
    numChunks = cdiv fileSize chunkSize
    chunks = V.fromList do
      big <- resegmentPadded chunkSize (Lazy.toChunks content)
      byteStringToVector <$> chunkedBy chunkSize big
