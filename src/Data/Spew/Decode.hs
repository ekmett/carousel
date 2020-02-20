module Data.Spew.Decode
  ( suck
  ) where

import Data.Spew.Codec

data Decoder =
  Decoder
  { decoderCodec :: Codec
  , acceptFile :: FileName -> Int64 -> IO Bool
  }

packetFilter :: WrappedPacket -> IO ()

decode :: Codec -> IO (Packet -> IO ())
decode 

suck :: MonadIO m => PortNumber -> IO Socket
suck port = do
  sock <- socket AF_INET Datagram 0
  addr <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE }) Nothing (Just "12345"
  

spew :: MonadIO m => Codec -> SockAddr -> Bool -> 
   m (Strict.ByteString -> Lazy.ByteString -> IO ())
spew codec sockaddr broadcast = liftIO do
  sock <- socket AF_INET Datagram 0

