{-# Language PatternSynonyms #-}
{-# Language BlockArguments #-}
module Data.Spew.Decode
  ( suck
  , Sucker(..)
  ) where

import Control.Monad (unless)
import Data.HashMap.Strict as HM
import Data.IORef
import Data.Spew.Codec
import Data.Spew.Filter
import Data.Vector as V
import Data.Vector.Mutable as MV

data Sucker = 
  forall s. Sucker
  { serviceName :: String
  , packetFilter :: Filter Packet
  , fileHandler :: FileName -> Lazy.ByteString -> IO ()
  }

pattern MTU :: Int
pattern MTU = 8192

suck :: MonadIO m => Sucker -> IO ThreadId
suck s = liftIO do
  sock <- socket AF_INET Datagram 0
  addr:_ <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE }) Nothing (Just servicePort)
  bind sock $ addrAddress addr
  state <- newIORef defaultProcessorState
  forkIO $ forever do
    (packet, sender) <- recvFrom sock MTU
    runGet get packet >>= \case
      Left s  -> putStrLn $ "corrupt packet: " ++ s
      Right p -> do
        ok <- packetFilter s packet sender
        when ok do
          process state s p sender

type ProcessorState = HashMap FileName FileState

defaultProcessorState :: HashMap FileName FileState
defaultProcessorState = mempty

data FileState
  = FileState
  { fileSize       :: {-# UNPACK #-} !Word64
  , fileCodec      :: {-# UNPACK #-} !Codec
  , fileData       :: {-# UNPACK #-} !(MV.MVector Assembly)
  , filePending    :: {-# UNPACK #-} !(IORef Word64)
  , fileDone       :: {-# UNPACK #-} !(IORef Bool)
  } 

data Assembly
  = Fragments {-# UNPACK #-} !Word8 [Fragment]
  | Assembled [ByteString]
  deriving Show

data Fragment = Fragment 
  { fragmentShardId :: {-# UNPACK #-} !Word8
  , fragmentPayload :: {-# UNPACK #-} !ByteString
  } deriving Show

advance :: FilePath -> Sucker -> FileState -> IO ()
advance fp s FileState{..} = do
  o <- modifyIORef' filePending \n -> (n-1,n-1)
  when (o == 0) do
    let n = MV.length fileData
        go acc i
          | i >= 0 = do
            Assembled xs <- MV.read fileData i
            go (xs ++ acc) $! i-1
          | otherwise = pure acc
    lbs <- go acc (n-1)
    fileHandler s fp (Lazy.fromChunks lbs)
    writeIORef fileDone True

insertFragment :: Codec -> FilePath -> Sucker -> FileState -> Packet -> IO ()
insertFragment c fp s fd p = do
  let cid = packetChunkId p
  MV.read (fileData fd) cid >>= \case
    Assembled{} -> pure ()
    Fragments n fs
      | any (\f' -> fragmentShardId f' == sid) fs -> pure ()
      | n == 1 -> do
        MV.write (fileData fd) cid $ Assembled $ assemble c (f:fs)
        advance fp s fd
      | otherwise -> do
        MV.write (fileData fd) cid $ Fragments (n-1) (f:fs)
        advance fp s fd
      where sid = packetShardId p
            f = packetFragment p
       
assemble :: Codec -> [Fragment] -> [ByteString]
assemble = error "TODO"

packetFragment :: Packet -> Fragment
packetFragment Packet{..} = Fragment packetShardId packetPayload

initFileState :: Packet -> IO FileState
initFileState Packet{..} = do
  let fileSize = packetFileSize
      ds = packetDataShards 
      fileCodec = Codec.new ds -- TODO use a cache
      chunks = cdiv fileSize (PAYLOAD_SIZE*ds)
  filePending <- newIORef (chunks*ds) -- count down to done
  fileData <- MV.replicate chunks $ Fragments ds []
  pure FileState{..}

process :: IORef ProcessorState -> Sucker -> Packet -> SockAddr -> IO ()
process iops sucker packet sender = do
  fhm <- readIORef iops
  HM.lookup (packetFileName packet) fhm >>= \case
    Nothing -> do
       initFileState
       insertFragment c fp s fd p
    Just fs -> do
      -- TODO: lint for sanity to make sure the parameters match
      <- MV.read (fileData fs) (fromIntegral $ packetChunkId p)
  
  
