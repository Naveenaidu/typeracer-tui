{-# LANGUAGE OverloadedStrings #-}
module Server.Types where

import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM (STM, TVar, TChan, newTVar, newTVarIO, newTChanIO, newBroadcastTChan, newBroadcastTChanIO)
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

data Score = Score { accuarcy :: Int
                   , wpm :: Int
                   } deriving (Show,Ord,Eq)

-- | The Typeracer game is the client 
-- | Handle is equivalent to the socket to which the users will be connected to
-- | A Chan is an unbounded Message Queue, which stores the message object it receives from
-- | other clients.
data Client = Client { clientUser         :: !User
                     , clientHandle       :: !Handle
                     , clientPongTime     :: MVar UTCTime
                     , clientChan         :: TChan Message
                     , clientScore        :: Score
                     } deriving Eq

newClient :: User -> Handle -> IO Client
newClient user handle = do
  now             <- getCurrentTime
  clientPongTime  <- newMVar now
  clientChan      <- newTChanIO
  let score = Score 0 0
  return $ Client user handle clientPongTime clientChan score

-- Private Room is used to group clients together so that they can complete in a match
-- Each Room will primary have the information about the sockets that are connected to it
-- MaxUsers is used as a password to begin the game when the
-- MIGHT HAVE TO CHANGE THE TYPES

-- NOTE: For now use Handle, if it does not work - switch back to sockets
data PrivateRoom = PrivateRoom { roomName     :: !RoomName
                               , roomUsers    :: TVar [Client]
                               , roomMaxUsers :: Int
                               , roomSockets  :: TVar [Handle]
                               , roomChan     :: TChan Message
                               , roomUsersGameEnd :: Int
                               }

-- A private room is created using the hash of the room name, the main user in the set who called the
-- CREATE Room event and their socket info.
newPrivateRoom :: RoomName -> Client -> Handle -> Int -> STM PrivateRoom
newPrivateRoom roomName user handle maxUsers= do
  roomUsers <- newTVar $  [user]
  roomSockets <- newTVar $ [handle]
  roomChan  <- newBroadcastTChan
  return $ PrivateRoom roomName roomUsers maxUsers roomSockets roomChan 0

-- A server is a mutable map between User and client
data Server = Server { serverUsers :: MVar (Map.Map User Client) 
                     , serverRooms :: TVar (Map.Map RoomName PrivateRoom)
                     }

newServer :: IO Server
newServer = do
  serverUsers    <- newMVar Map.empty
  serverRooms <- newTVarIO Map.empty
  return $ Server serverUsers serverRooms

data Message = -- Server messages
               NameInUse UserName
             | RoomNameInUse RoomName
             | RoomNotExist RoomName
             | RoomNotFull RoomName
             | RoomFull RoomName
             | Ping
             | RoomCreated RoomName
             | JoinedRoom RoomName UserName
             | Wait
             | Leaved RoomName User
             | MatchStart RoomName 
             | MatchEnd RoomName Int Int
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
