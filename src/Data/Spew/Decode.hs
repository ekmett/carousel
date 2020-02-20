module Data.Spew.Decode
  (

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
