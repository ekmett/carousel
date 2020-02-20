{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}
module Data.Carousel.Scatter
  ( encodePackets
  , scatter
  ) where

import Control.Monad (when)
import Control.Loop (numLoop)
import Control.Monad.IO.Class
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Foldable as Foldable
import Data.Serialize
import Data.Carousel.Codec
import Data.Carousel.Internal
import Data.Carousel.Packet
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

encodePayload :: GivenCodec => Int -> Chunk -> Vec
encodePayload shard chunk
  | shard < dataShards = S.slice (payloadSize*shard) payloadSize chunk
  | otherwise = S.create do
  mv <- MS.new payloadSize
  let r = S.drop (dataShards*shard) code
  gvmul mv (S.head r) chunk
  mv <$ numLoop 1 (S.length r - 1) \i ->
    gvfma mv (r S.! i) $ S.drop (payloadSize*i) chunk

-- packets could be saved for continuous distribution for instance
encodePackets :: GivenCodec => Strict.ByteString -> Lazy.ByteString -> [Packet]
encodePackets fileName content = do
  shard <- [0..SHARDS-1]
  chunk <- shuffleWith shard numChunks
  pure Packet
    { payload = vectorToByteString $ encodePayload shard $ chunks V.! chunk
    , shards = dataShards
    , ..
    }
  where
    fileSize = fromIntegral $ Lazy.length content
    chunkSize = payloadSize * dataShards
    numChunks = cdiv fileSize chunkSize
    chunks = V.fromList do
      big <- resegmentPadded chunkSize (Lazy.toChunks content)
      byteStringToVector <$> chunkedBy chunkSize big

-- eventually do something other than ^C to quit here
scatter :: (MonadIO m, GivenCodec) => SockAddr -> Bool ->
   m (Strict.ByteString -> Lazy.ByteString -> IO ())
scatter sockaddr broadcast = liftIO do
  System.IO.putStrLn "Opening socket"
  sock <- socket AF_INET Datagram 0
  when broadcast $ setSocketOption sock Broadcast 1
  connect sock sockaddr
  return \fileName content -> do
    System.IO.putStrLn "encoding packets"
    let packets = encodePackets fileName content
    print packets
    System.IO.putStrLn "sending packets"
    Foldable.forM_ packets $
      Strict.send sock . runPut . put . Hashed
