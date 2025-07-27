{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Data.Functor
import Network.Socket
import qualified ProtoUtils as PU
import System.Environment

main :: IO ()
main = findSocketPath >>= putToLog >>= runWith queryLoop
  where
    queryLoop sock = respondToQuery sock >> queryLoop sock

putToLog :: (Show a) => a -> IO a
putToLog a = print a >> return a

respondToQuery :: Socket -> IO ()
respondToQuery sock = PU.readQuery sock >>= PU.processQuery >>= PU.writeResponse sock

-- | Find a path to bind to.
findSocketPath :: IO String
findSocketPath =
  lookupEnv "XDG_RUNTIME_DIR" <&> \case
    Nothing -> "/tmp/datalog.sock"
    Just sockPath -> sockPath <> "/datalog.sock"

-- | Start listening on a UNIX socket, running one (userspace) thread for each connection.
runWith :: (Socket -> IO ()) -> String -> IO ()
runWith handleConn sockPath = do
  sock <- openSocket sa
  bind sock (addrAddress sa)

  -- Queue 4 connections. This is an arbitrary pick and may need to change in the future.
  listen sock 4
  connLoop sock
  where
    connLoop sock = do
      (client, _) <- accept sock
      _ <- forkIO $ handleConn client >> close client
      connLoop sock

    sa =
      AddrInfo
        { addrFlags = [],
          addrFamily = AF_UNIX,
          addrSocketType = Stream,
          addrProtocol = defaultProtocol,
          addrAddress = SockAddrUnix sockPath,
          addrCanonName = Nothing
        }
