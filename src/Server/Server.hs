{-# LANGUAGE RecordWildCards #-}

module Server.Server where

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Extra
import System.Random

import Network.Socket     ( AddrInfo (..), AddrInfoFlag(..), SocketType(..)
                          ,HostName(..), withSocketsDo, accept
                          , socketToHandle, defaultHints, getAddrInfo, socket
                          , bind, listen)
import System.IO          (hClose, hSetNewlineMode, hSetBuffering, BufferMode(..),
                           IOMode(..), universalNewlineMode, hGetLine, Handle, stdout)
import System.Timeout     (timeout)
import Text.Printf        (printf)
import Data.Time              (UTCTime, getCurrentTime)


import qualified Data.Map.Strict as Map
import Data.List

import Server.Client
import Server.Types
import Server.Protocol

-- | Types used for communicating between functions
-- Tells about the state of the match
data MatchState = MatchHasEnded
                | MatchRunning
                | RoomIsFull
                | RoomNotFull
                | ClientJoinedRoom
                | RoomDoesNotExist


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
      -- Wait for 10 min before disconnecting the client.
      -- TODO: Do we need this, since we are using `gracefullyShutdown` functions??
      waitDelay       = 600
      waitDelayMicros = waitDelay * 1000 * 1000

      readCommand = do
        command <- timeout waitDelayMicros . fmap parseCommand $hGetLine handle
        printf "Command read\n" 
        
        case command of
          -- Connection TimedOut
          Nothing -> printf "Client login timed out \n" >> return ()

          -- Parsed a valid command
          Just (Just (command')) -> do
            void $ forkIO (procClientCommand server handle command') 
            readCommand
          
          -- Parsed an Invalid Command
          _ -> do
            printf "Error: Invalid Command \n" 
            readCommand

-- Process the command received by client
procClientCommand :: Server -> Handle -> Message -> IO ()
procClientCommand server handle command = do
  case command of
    (CreateRoom userName maxUsers)    -> createRoom server handle userName maxUsers
    (JoinRoom roomName userName)      -> joinRoom server handle userName roomName
    (GameEnd roomName userName accuracy wpm)  -> do
      gameEndTime <- getCurrentTime
      endGame server handle roomName userName accuracy wpm gameEndTime
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
  -- If Room does not exist create the room
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
  clients <- readTVar (roomClients room)
  let maxUsers = (roomMaxUsers room)
  return (length clients >= maxUsers)

-- | Checks to run, before we join someone in room
-- TODO: Do we really need the `RoomNotFull` Message type.
checkBeforeJoinRoom :: Server -> RoomName -> IO MatchState
checkBeforeJoinRoom Server{..} roomName = atomically $ do
  -- Check if room exists 
  roomMap <- readTVar serverRooms

  case Map.lookup roomName roomMap of
    -- If room does not exist
    Nothing -> return RoomDoesNotExist
    -- If Room exist, check if room reached it's max capacity or if the match has already started
    -- The 'isMatchStarted' case is necessary for the scenario where a client disconnects/drops out of match 
    -- and wishes to join again. In those cases, Do not allow that to happen.
    Just room -> do
      roomFull <- isRoomFull room
      isMatchStarted <- readTVar (matchStarted room)
      if (isMatchStarted || roomFull) 
        then return RoomIsFull
        else return RoomNotFull

-- | Join the client to room
-- | Add the user to map and return the list of client to whom join message needs to be sent)
-- | If the number of players in the room equal maxUsers, spawn a new thread to start the game
-- | ASK: How to get rid of `nested if` :(
joinClientToRoom :: Server -> Handle -> Client -> RoomName -> IO (MatchState, Maybe [Client])
joinClientToRoom Server{..} handle client@Client{..} roomName = atomically $ do
  -- Fetch the roomMap from the server
  roomMap <- readTVar serverRooms

  -- If the room exists, add the user to the room and add the handle to the room
  case Map.lookup roomName roomMap of
    Just room -> do

      -- Check if the room is full before adding the client
      roomFull <- isRoomFull room
      if (roomFull) 
       then do  return $ (RoomIsFull, Nothing)
       else do
        -- Room Not Full: Add client to the room
        modifyTVar (roomClients room) $ (:) client
        modifyTVar (roomSockets room) $ (:) handle

        -- Check if the room is full `after` adding the client
        roomFull' <- isRoomFull room
        usersToNotify <- usersForJoinNotification room client
        if (roomFull')
          -- If room is full then start the match
          then do
            -- Update the status of room to represent match started
            writeTVar (matchStarted room) True
            return $ (RoomIsFull, Just usersToNotify)
          -- If room is not full send join notification to all the clients
          -- else return $ JoinedRoom roomName (userName clientUser)
          else do
            return $ ( ClientJoinedRoom, Just usersToNotify)

-- | Send message to Chan
-- | ASK: What's the benefit of writing data to a TChan.
-- sendMessageRoom :: PrivateRoom -> Message -> STM ()
-- sendMessageRoom PrivateRoom{..} = writeTChan roomChan

-- -- | Send join notification to all the other clients in the room
usersForJoinNotification :: PrivateRoom -> Client -> STM [Client]
usersForJoinNotification PrivateRoom{..} joinedClient = do
  -- Get all the clients in the room
  users <- readTVar roomClients

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
    RoomDoesNotExist -> printToHandle  handle.formatMessage $ RoomNotExist roomName
    RoomIsFull ->  printToHandle  handle.formatMessage $ RoomFull roomName
    
    -- If no errors, then join the user to the room
    RoomNotFull -> do
      okClient <- checkAddClient server handle userName
      case okClient of
        Nothing -> printToHandle  handle.formatMessage $ NameInUse userName

        -- Join the client to room
        Just client -> do
          joinClientToRoomMessage <- joinClientToRoom server handle client roomName
          case joinClientToRoomMessage of
            (RoomIsFull, Nothing)  -> sendMessage client  (RoomFull roomName)
            -- Send Match Start message
            (RoomIsFull, Just usersToNotify) ->  do 
              sequence_ $ sendMessageToUsers usersToNotify (JoinedRoom roomName userName)
              sequence_ $ sendMessageToUsers (client:usersToNotify) (RoomFull roomName)
              sequence_ $ sendMessageToUsers (client:usersToNotify) (MatchStart roomName)
              -- A thread to end the match if there is no response after `n` seconds. `n` represents the timer in game
              -- TODO: Parameterized this timer. For now, I'll assume it's (3 minutes == 180 seconds)
              gracefullyEndMatch 180 server roomName
            (ClientJoinedRoom, Just usersToNotify') -> do
              sequence_ $ sendMessageToUsers usersToNotify' (JoinedRoom roomName userName)

-- Check if all the players have completed playing the game
-- This check is done after the user is added to the room
isMatchEnded :: PrivateRoom -> STM Bool
isMatchEnded room = do
  numClients <- readTVar (numClientsGameEnd room)
  let maxUsers = (roomMaxUsers room)
  let matchState = numClients == maxUsers
  -- Update the status of match for the room
  writeTVar (matchEnded room) matchState
  return (numClients == maxUsers)


-- Update the score of the client
updateClientState :: UserName -> Int -> Int -> UTCTime ->  [Client] -> IO [Client]
updateClientState userName' accuracy' wpm' gameEndTime clients  = 
  mapM (updateScore userName' accuracy' wpm') clients  
  where 
    updateScore userName' accuracy' wpm' client =
      -- If username is persent, then return the client with updated score, 
      -- else return the client unchanged
      if ((userName $ clientUser client) == userName')
        then do 
          let score = Score{accuracy=accuracy', wpm=wpm', time=gameEndTime}
          atomically $ writeTVar (clientGameEnd client) True
          return client{clientScore = score}
        else return client

-- | End the game for the client
-- Return Nothing if match is not ended, else return Just MatchEnd
endClientGame :: PrivateRoom -> UserName -> Int -> Int -> UTCTime -> IO MatchState
endClientGame room userName' accuracy wpm gameEndTime = do
  clientsInRoom <- readTVarIO $ roomClients room
  updatedClients <-  updateClientState userName' accuracy wpm gameEndTime clientsInRoom
  atomically $ do
    -- Update the score of the client present in the room, also update the game state of the client
    writeTVar (roomClients room) $ updatedClients

    -- Update the number of players who have finished the game
    modifyTVar' (numClientsGameEnd room) (+1)  

    -- Check if all the players have finished the game
    -- If yes, then send `GAME END` message and initiate the Score Sending procedure
    isMatchEnded' <- isMatchEnded room

    if (isMatchEnded')
      then return MatchHasEnded
      else return MatchRunning

-- When `END MATCH` command is recived from client. Update the number of players for which game has 
-- ended variable in the private room. And also update the score of the user.
-- When all the players have finished the game, send the scores to all the users. That might mean
-- We might have to spawn a new thread to handle it.
-- TODO: Allow players to re-play the game in the same room
endGame :: Server -> Handle -> RoomName -> UserName -> Int -> Int -> UTCTime -> IO ()
endGame server@Server{..} handle roomName userName' accuracy wpm gameEndTime =  do
  -- Check if room exists 
  roomMap <- atomically $ readTVar serverRooms 
  case Map.lookup roomName roomMap of
    -- If room does not exist, This case will not exist I guess
    Nothing -> return ()

    -- End the match for the client and update the score and check the state of the match
    Just room -> do
      matchState <- endClientGame room userName' accuracy wpm gameEndTime
      case matchState of
        MatchHasEnded -> do
          endMatch room
          -- Remove the room from the server map
          removePrivateRoom server roomName
          -- Remove all the clients from the server map
          clients <-  readTVarIO $ roomClients room
          sequence_ $ map (removeClient server . clientUser) clients
        MatchRunning -> return ()
      
-- Rank the clients in the `increasing` order of their timestamp
-- Ranking needs to be done, on the basis of the timestamp the game was completed
-- Compare clients on the field `timestamp`
-- Note: sortBy sorts in increasing order, hence using flip to reverse the sorting
-- `flip` reverses the order of arguments
rankClients :: [Client] -> [(Int, Client)]
rankClients clients = zip [1,2 ..] clientsSortedByScore 
  where
    clientsSortedByScore = sortBy compareTime clients  
    compareTime client1 client2 = 
      compare  clientTime1 clientTime2
      where clientTime1 = time $ clientScore client1
            clientTime2 = time $ clientScore client2

-- Send a list of messages to client
sendScoreToClient :: [Message] -> Client -> IO ()
sendScoreToClient scoreBoard client = do
  mapM_ (sendMessage client) scoreBoard

-- Send Scores to Client
-- Rank the scores of the clients, and send them in decreasing order
sendScoresToClients :: [Message] -> [Client] -> IO ()
sendScoresToClients scoreBoard clients  = do
  mapM_ (sendScoreToClient scoreBoard) clients

-- Prepare the list of Messages that contains the score of all Clients
prepareScoreBoard :: RoomName -> [Client] -> IO [Message]
prepareScoreBoard roomName clients = do
  let rankedClients = rankClients clients
  return $ map prepareMessages rankedClients
  where
    prepareMessages (rank, client) = do
      let userName' = userName (clientUser client)
      SendScore roomName userName' rank (clientScore client)
      

-- Send MatchEnd message to all clients
-- Send Scoreboard to all clients
-- Disconnect all users
endMatch :: PrivateRoom -> IO ()
endMatch room = do
  let roomName' = roomName room
  clients <-  readTVarIO $ roomClients room
  scoreBoard <- prepareScoreBoard roomName' clients
  let numClients = length clients
  sequence_ $ sendMessageToUsers clients (MatchEnd roomName')
  sequence_ $ sendMessageToUsers clients (SendScoreStart roomName' numClients )
  sendScoresToClients scoreBoard clients 
  sequence_ $ sendMessageToUsers clients (SendScoreEnd roomName' numClients)

removePrivateRoom :: Server -> RoomName -> IO ()
removePrivateRoom Server{..} room =
  atomically $ modifyTVar serverRooms $ Map.delete room

-- | End the match gracefully after the timer runs out.
-- TODO: Make sure that the delay is atleast few seconds more than the actual game timer.
-- This thread is usefull, when the match did not end properly.
-- This is used for scenarios where a clients might have accidentally dropped from the match or where
-- a client does not responsd. So instead of making all the players wait - Just send a timeout message to the unresponsive player.
-- Once the game starts, start the timer of `delay` seconds. Once the timer runs out, check if the match is still ongoing,
-- if the match is still ongoing then, the problem might be an unresponsive client, send a timeout message to them and scoreboard to
-- rest of the players
gracefullyEndMatch :: Int -> Server -> RoomName -> IO ()
gracefullyEndMatch delayInSec server@Server{..} roomName = do
  -- Dealy the thread/ Wait for the game to finish
  -- Delaying here is equivalent to the timer that runs in the game. It's like once the timer runs out in
  -- the game, this thread is activated.timeout waitDelayMicros . fmap parseCommand $hGetLine handle
  threadDelay delayInMicros

  -- Check if the room exists in servertimeout waitDelayMicros . fmap parseCommand $hGetLine handle map
  -- If the room does not exist, that means that the match has ended successfully
  roomMap <- atomically $ readTVar serverRooms 
  case Map.lookup roomName roomMap of
    Nothing -> return ()

    -- Room in still present in the Map, means some client is blocking the match from being ended
    Just room -> do

      -- Get the clients from the room
      clients <- readTVarIO $ roomClients room
      
      -- ASK: What will happen, if I also call the `endMatch` function? Let's see :P
      -- Check if the match has ended, if it has then could be that the 'endMatch' function is working.
      matchEnded' <- atomically$  readTVar $ matchEnded room
      if matchEnded' 
        then endMatch room
        -- If the match has not ended, then gracefully end match
        else do
          -- Parition the client to get (clients finished the game, clients did not finish the game)
          -- NOTE: We used paritionM instead of parition because, the `func` returned a `m Bool` 
          -- NOTE: The `M` stands for Monadic version of functions.
          (gameEndedClients,gameRunningClients) <- partitionM  (readTVarIO . clientGameEnd) clients
          
          --  Send a `TimedOut` notification to players who did not finish the game
          sequence_ $ sendMessageToUsers gameRunningClients (TimeOut roomName)

          
          -- TODO: Code Repetition from 'endGame' function. Refractor this.
          -- Send the ScoreBoard to all the players, removing the players who did not finish the game.
          let numClients = length gameEndedClients
          -- Prepare the scoreboard only for the players who finished the game
          scoreBoard <- prepareScoreBoard roomName gameEndedClients
          sequence_ $ sendMessageToUsers clients (MatchEnd roomName)
          sequence_ $ sendMessageToUsers clients (SendScoreStart roomName numClients )
          sendScoresToClients scoreBoard clients 
          sequence_ $ sendMessageToUsers clients (SendScoreEnd roomName numClients)

          -- Remove the room from the server map
          removePrivateRoom server roomName
          -- Remove all the clients from the server map
          sequence_ $ map (removeClient server . clientUser) clients

  where
    delayInMicros = delayInSec * 1000 * 1000
