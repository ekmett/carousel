{-# Language BlockArguments #-}
{-# Language RecordWildCards #-}
module Main where

import Data.ByteString.Lazy as Lazy
import Data.Foldable as Foldable
import Data.IP
import Data.Spew
import Data.Spew.Codec as Codec
import Data.String
import Network.Socket
import Options.Applicative

data Options
  = Options
  { ip        :: !IP
  , port      :: !PortNumber
  , broadcast :: !Bool
  , payload   :: {-# UNPACK #-} !Int
  , dshards   :: {-# UNPACK #-} !Int
  , files     :: [FilePath]
  }

options :: Parser Options
options = Options 
  <$> option auto
    ( long "host"
   <> short 'h'
   <> help "target IP address"
   <> value (read "127.0.0.1")
   <> showDefault
   <> metavar "HOST")
  <*> option auto
    ( long "port"
   <> short 'p'
   <> help "target port number"
   <> showDefault
   <> value 20202
   <> metavar "PORT" )
  <*> switch 
    ( long "broadcast"
   <> short 'b'
   <> help "use a broadcast port")
  <*> option auto
    ( long "payload"
   <> short 'l'
   <> help "packet payload size"
   <> metavar "BYTES"
   <> showDefault
   <> value 1024)
  <*> option auto
    ( long "data-shards"
   <> short 'd'
   <> help "number of data shards"
   <> showDefault
   <> value 16
   <> metavar "INT" )
  <*> some (argument str (metavar "FILES..."))

mainOptions :: ParserInfo Options
mainOptions = info (options <**> helper)
  $ fullDesc
 <> progDesc "spew files over the internet"
 <> header "spew"
  
main :: IO ()
main = do
  Options{..} <- execParser mainOptions
  Codec.with (Codec.new payload dshards) do
    emit <- spew (toSockAddr (ip,port)) broadcast
    Foldable.forM_ files \file -> do
      Prelude.putStrLn $ "reading " ++ show file
      content <- Lazy.readFile file
      Prelude.putStrLn $ "sending " ++ show file
      emit (fromString file) content
      Prelude.putStrLn $ "sent " ++ show file

