{-# LANGUAGE OverloadedStrings #-}
module Server.Types where

import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM (STM, TVar, TChan, newTVar, newTVarIO, newTChanIO, newBroadcastTChanIO)
import Data.Time              (UTCTime, getCurrentTime)
import System.IO              (Handle)
import Network.Socket

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T

type UserName = String
type RoomName = String

data User = User {userName :: !UserName}
              deriving (Show, Ord, Eq)

-- | The Typeracer game is the client 
-- | Handle is equivalent to the socket to which the users will be connected to
-- | A Chan is an unbounded Message Queue, which stores the message object it receives from
-- | other clients.
data Client = Client { clientUser         :: !User
                     , clientHandle       :: !Handle
                     , clientPongTime     :: MVar UTCTime
                     , clientChan         :: TChan Message
                     }

newClient :: User -> Handle -> IO Client
newClient user handle = do
  now             <- getCurrentTime
  clientPongTime  <- newMVar now
  clientChan      <- newTChanIO
  return $ Client user handle clientPongTime clientChan

-- Private Room is used to group clients together so that they can complete in a match
-- Each Room will primary have the information about the sockets that are connected to it
-- MaxUsers is used as a password to begin the game when the
-- MIGHT HAVE TO CHANGE THE TYPES

-- NOTE: For now use Handle, if it does not work - switch back to sockets
data PrivateRoom = PrivateRoom { roomName     :: !RoomName
                               , roomUsers    :: MVar (Set.Set Client)
                               , roomMaxUsers :: Int
                               , roomSockets  :: MVar (Set.Set Handle)
                               , roomChan     :: TChan Message
                               }

-- A private room is created using the hash of the room name, the main user in the set who called the
-- CREATE Room event and their socket info.
newPrivateRoom :: RoomName -> Client -> Handle -> Int -> IO PrivateRoom
newPrivateRoom roomName user handle maxUsers= do
  roomUsers <- newMVar $ Set.singleton user
  roomSockets <- newMVar $ Set.singleton handle
  roomChan  <- newBroadcastTChanIO
  return $ PrivateRoom roomName roomUsers maxUsers roomSockets roomChan

-- A server is a mutable map between User and client
data Server = Server { serverUsers :: MVar (Map.Map User Client) 
                     , serverRooms :: MVar (Map.Map RoomName PrivateRoom)
                     }

newServer :: IO Server
newServer = do
  serverUsers    <- newMVar Map.empty
  serverRooms <- newMVar Map.empty
  return $ Server serverUsers serverRooms

data Message = -- Server messages
               NameInUse UserName
             | RoomNameInUse RoomName
             | Ping
             | RoomCreated RoomName
             | JoinedRoom RoomName UserName
             | Wait
             | Leaved RoomName User
             | MatchStart RoomName 
             | MatchEnd RoomName 
             | DisplayScores RoomName
             | InvalidMessage T.Text
               -- Client messages
             | Pong
             | Login UserName
             | Leave RoomName
             | CreateRoom UserName Int
             | JoinRoom RoomName UserName
             | Quit
               deriving (Show, Eq)
