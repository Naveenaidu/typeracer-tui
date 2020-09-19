module Server.Server where


import Control.Concurrent
import Control.Exception  hiding (handle)
import Control.Monad      (forever, join, void)
import Network.Socket     ( AddrInfo (..), AddrInfoFlag(..), SocketType(..)
                          , SocketOption(..),HostName(..), withSocketsDo, accept
                          , socketToHandle, defaultHints, getAddrInfo, socket
                          , setSocketOption, bind, listen)
import System.IO          (hClose, hSetNewlineMode, hSetBuffering, BufferMode(..),
                           IOMode(..), universalNewlineMode, hGetLine, Handle, stdout)
import System.Timeout     (timeout)
import Text.Printf        (printf)

import qualified Data.Map.Strict as Map

import Server.Client
import Server.Types
import Server.Protocol

runServer :: HostName -> Int -> IO ()
runServer host port = withSocketsDo $ do
  -- Set the handle for stdout for LineBuffering, i.e buffer a full line 
  hSetBuffering stdout LineBuffering

  -- Create a empty server object
  server <- newServer
  sock <- newSocket
  printf "Listening on port%d\n" port
  forever $ do
    -- Accept the connections coming on the master sock
    (sock', addr) <- accept sock
    printf "Accepted connection from %s\n" (show addr)

    -- Conver the socket to handle. This helps in managing sockets better.
    -- TODO: But How??
    handle <- socketToHandle sock' ReadWriteMode

    -- Create a new thread. And transfer the sock to the thread to handle the connection
    -- This helps us in taking in multiple connections without blocking the master sock
    void $ forkFinally (connectClient server handle) (\_ -> hClose handle)
  where
    newSocket = do
      -- Hints to derive the address information.
      let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
                               , addrSocketType = Stream }

      -- Look up the port. Either raises an exception or returns a non-empty list.
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just (show port))
      
      -- Create a socket
      sock   <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

      -- Bind the socket to the address we are listening to
      bind sock (addrAddress addr)

      -- Start listening for the connection requests. Maximum queue size of 10 connection
      -- requests waiting to be accepted
      listen sock 10
      return sock

-- Connect to client and parse the commands from client
connectClient :: Server -> Handle -> IO ()
connectClient server handle = do
  -- Handle all different cases of carriage return (`\r\n`, `\n`)
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering

  -- Read the command comming from the client after joining
  readCommand
    where
      waitDelay       = 60
      waitDelayMicros = waitDelay * 1000 * 1000

      readCommand = do
        command <- timeout waitDelayMicros . fmap parseCommand $hGetLine handle
        printf "Command read\n"
        case command of
          Nothing -> printf "Client login timed out \n" >> return ()
          Just (Just (CreateRoom userName maxUsers)) -> do
            printToHandle handle . formatMessage $ RoomCreated userName
            readCommand
          _ -> readCommand
