{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
module Data.Carousel.Gather
  ( suck
  , Sucker(..)
  ) where

import Control.Monad (unless)
import Data.Carousel.Codec
import Data.Carousel.Filter

data Sucker =
  forall s. Sucker
  { serviceName :: String
  , packetFilter :: Filter Packet
  , defaultState :: s
  , fileHandler :: FileName -> Lazy.ByteString -> IO ()
  , process :: Packet -> IORef s -> IO ()
  }

pattern MTU :: Int
pattern MTU = 8192

suck :: MonadIO m => Sucker -> IO ThreadId
suck s = do
  sock <- socket AF_INET Datagram 0
  addr:_ <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE }) Nothing (Just servicePort)
  bind sock $ addrAddress addr
  state <- newIORef (defaultState s)
  forkIO $ forever do
    (packet, sender, ok) <- liftIO $ do
      (packet, sender) <- recvFrom sock MTU
      ok <- packetFilter s packet sender
      return (packet, sender, ok)
    when ok do
      process s state packet
