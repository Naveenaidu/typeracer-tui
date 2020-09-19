{-# LANGUAGE RecordWildCards #-}

module Server.Server where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import System.Random

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
          Just (Just (command')) -> do
            void $ forkIO (procClientCommand server handle command') 
            readCommand
          _ -> readCommand

-- Process the command received by client
procClientCommand :: Server -> Handle -> Message -> IO ()
procClientCommand server handle command = do
  case command of
    (CreateRoom userName maxUsers) -> createRoom server handle userName maxUsers
    (JoinRoom roomName userName)   -> undefined

checkAddClient :: Server -> Handle -> UserName -> IO (Maybe Client)
checkAddClient Server{..} handle userName = do
  -- Make a new user
  let user = User userName

  -- Add the User to the Server Map
  modifyMVar serverUsers $ \clientMap ->
    if Map.member user clientMap
      then return (clientMap, Nothing)
      else do
        client <- newClient user handle
        printf "New user connected: %s\n" userName
        return (Map.insert user client clientMap, Just client)

-- Check and Map Client to the Private Room
checkAddClientToRoom :: Server -> Handle -> Client -> RoomName -> Int -> IO (Maybe PrivateRoom)
checkAddClientToRoom Server{..} handle client roomName maxUsers = do
  modifyMVar serverRooms $ \roomMap ->
        if Map.member roomName roomMap
          then return (roomMap, Nothing)
          else do
            privateRoom <- newPrivateRoom roomName client handle maxUsers
            printf "New Room created: %s, with User: %s\n" roomName (userName $ clientUser client)
            return (Map.insert roomName privateRoom roomMap, Just privateRoom)

createRoom :: Server -> Handle -> UserName -> Int -> IO ()
createRoom server@Server{..} handle userName maxUsers = do

  -- Generate a random room name
  roomName <- replicateM 15 (randomRIO ('a', 'z'))

  -- Check and add the client to the Client Map server maintains
  okClient <- checkAddClient server handle userName 
  case okClient of

    -- If the name exists, print an error saying that it exits
    -- TODO: Prompt the user for a new name
    Nothing -> printToHandle  handle.formatMessage $ NameInUse userName
    
    -- Add the client to the private room
    Just client -> do

      okRoom <- checkAddClientToRoom server handle client roomName maxUsers
      case okRoom of
        Nothing -> printToHandle handle . formatMessage $ RoomNameInUse roomName
        Just room -> printToHandle handle . formatMessage $ RoomCreated roomName

joinRoom :: Server -> Handle -> UserName -> RoomName -> IO ()
joinRoom server@Server{..} handle userName roomName = do
  -- TODO: Add check to see if the user joining the room exceeds maxUser. If yes refuse the connection
  -- close the handle

  -- Check and add the client to the Client Map server maintains
  okClient <- checkAddClient server handle userName 
  case okClient of

    -- If the name exists, print an error saying that it exits
    -- TODO: Prompt the user for a new name
    Nothing -> printToHandle  handle.formatMessage $ NameInUse userName
    -- Add the client to the private room
    -- Fetch the room from serverRoom Map
    -- Update the `roomUser` of that PrivateRoom
    Just client -> undefined
      