{-# LANGUAGE RecordWildCards #-}

module Server.Server where

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM
import Control.Monad
import System.Random
import Control.Monad.IO.Class

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
import qualified Data.Set        as Set

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
      waitDelay       = 600
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
    (CreateRoom userName maxUsers)    -> createRoom server handle userName maxUsers
    (JoinRoom roomName userName)      -> joinRoom server handle userName roomName
    (MatchEnd roomName accuracy wpm)  -> endMatch server handle roomName accuracy wpm

checkAddClient :: Server -> Handle -> UserName -> IO (Maybe Client)
checkAddClient Server{..} handle userName = do
  -- Make a new user
  let user = User userName

  -- Add the User to the Server Map
  modifyMVar serverUsers $ \clientMap ->

    -- If user exists, then do not do anything
    if Map.member user clientMap
      then return (clientMap, Nothing)

      -- If user does not exists, then add user to the serverUser Map
      else do
        client <- newClient user handle
        printf "New user connected: %s\n" userName
        return (Map.insert user client clientMap, Just client)

removeClient :: Server -> User -> IO ()
removeClient Server{..} user =
  modifyMVar_ serverUsers $ return . Map.delete user 

checkAddClientToRoom :: Server -> Handle -> Client -> RoomName -> Int -> IO (Maybe PrivateRoom)
checkAddClientToRoom Server{..} handle client roomName maxUsers = atomically $ do
  roomMap <- readTVar serverRooms
  case Map.lookup roomName roomMap of
    
    -- If the room exist, Then it's a error
    Just room -> return Nothing

    -- If the room does not exist, create room and add client to the room
    Nothing -> do
      privateRoom <- newPrivateRoom roomName client handle maxUsers
      modifyTVar' serverRooms $ Map.insert roomName privateRoom
      return (Just privateRoom)

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

-- Check if the room will be full
-- This check is done after the user is added to the room
isRoomFull :: PrivateRoom -> STM Bool
isRoomFull room = do
  clients <- readTVar (roomUsers room)
  let maxUsers = (roomMaxUsers room)
  return (length clients >= maxUsers)

-- | Checks to run, before we join someone in room
-- TODO: Do we really need the `RoomNotFull` Message type.
checkBeforeJoinRoom :: Server -> RoomName -> IO (Maybe Message)
checkBeforeJoinRoom server@Server{..} roomName = atomically $ do
  -- Check if room exists 
  roomMap <- readTVar serverRooms

  case Map.lookup roomName roomMap of
    -- If room does not exist
    Nothing -> return $ Just $ RoomNotExist roomName
    -- If Room exist, check if room reached it's max capacity
    Just room -> do
      roomFull <- isRoomFull room
      if (roomFull) 
        then return $ Just $ RoomFull roomName
        else return $ Nothing

-- | Join the client to room
-- | Add the user to map and return the list of client to whom join message needs to be sent)
-- | If the number of players in the room equal maxUsers, spawn a new thread to start the game
-- | ASK: How to get rid of `nested if` :(
joinClientToRoom :: Server -> Handle -> Client -> RoomName -> IO (Message, Maybe [Client])
joinClientToRoom Server{..} handle client@Client{..} roomName = atomically $ do
  -- Fetch the roomMap from the server
  roomMap <- readTVar serverRooms

  -- If the room exists, add the user to the room and add the handle to the room
  case Map.lookup roomName roomMap of
    Just room -> do

      -- Check if the room is full before adding the client
      roomFull <- isRoomFull room
      if (roomFull) 
       then do  return $ (RoomFull roomName, Nothing)
       else do
        -- Room Not Full: Add client to the room
        modifyTVar (roomUsers room) $ (:) client
        modifyTVar (roomSockets room) $ (:) handle

        -- Check if the room is full `after` adding the client
        roomFull' <- isRoomFull room
        usersToNotify <- usersForJoinNotification room client
        if (roomFull')
          -- If room is full then spawn a new thread
          then do
            return $ (RoomFull roomName, Just usersToNotify)
          -- If room is not full send join notification to all the clients
          -- else return $ JoinedRoom roomName (userName clientUser)
          else do
            return $ ( JoinedRoom roomName (userName clientUser), Just usersToNotify)

-- | Send message to Chan
-- | ASK: What's the benefit of writing data to a TChan.
-- sendMessageRoom :: PrivateRoom -> Message -> STM ()
-- sendMessageRoom PrivateRoom{..} = writeTChan roomChan

-- -- | Send join notification to all the other clients in the room
usersForJoinNotification :: PrivateRoom -> Client -> STM [Client]
usersForJoinNotification PrivateRoom{..} joinedClient = do
  -- Get all the clients in the room
  users <- readTVar roomUsers

  -- Send notification of user joined to everyone in the room except the one who just joined
  let usersToNotify = [ u | u <- users, u /= joinedClient ]
  return usersToNotify

-- Helper function to send message to a client
sendMessage :: Client -> Message -> IO ()
sendMessage Client{..} = printToHandle clientHandle . formatMessage 

-- Helper function to send messages to a list of clients
sendMessageToUsers :: [Client] -> Message -> [IO ()]
sendMessageToUsers clients = mapM sendMessage clients

joinRoom :: Server -> Handle -> UserName -> RoomName -> IO ()
joinRoom server@Server{..} handle userName roomName = do
  checkBeforeJoin' <- checkBeforeJoinRoom server roomName
  case checkBeforeJoin' of
    -- Check if we can join the user to room
    Just (RoomNotExist _) -> printToHandle  handle.formatMessage $ RoomNotExist roomName
    Just (RoomFull _) ->  printToHandle  handle.formatMessage $ RoomFull roomName
    
    -- If no errors, then join the user to the room
    Nothing -> do
      okClient <- checkAddClient server handle userName
      case okClient of
        Nothing -> printToHandle  handle.formatMessage $ NameInUse userName

        -- Join the client to room
        Just client -> do
          joinClientToRoomMessage <- joinClientToRoom server handle client roomName
          case joinClientToRoomMessage of
            (RoomFull roomName, Nothing)  -> sendMessage client  (RoomFull roomName)
            (RoomFull roomName, Just usersToNotify) ->  do 
              sequence_ $ sendMessageToUsers usersToNotify (JoinedRoom roomName userName)
              sequence_ $ sendMessageToUsers (client:usersToNotify) (RoomFull roomName)
              sequence_ $ sendMessageToUsers (client:usersToNotify) (MatchStart roomName)
            (JoinedRoom roomName userName, Just usersToNotify') -> do
              sequence_ $ sendMessageToUsers usersToNotify' (JoinedRoom roomName userName)

endMatch :: Server -> Handle -> RoomName -> Int -> Int -> IO ()
endMatch server@Server{..} handle roomName accuracy wpm = undefined 
