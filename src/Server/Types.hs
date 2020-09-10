{-# LANGUAGE OverloadedStrings #-}
module Server.Types where

import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM (STM, TVar, TChan, newTVar, newTVarIO, newTChanIO, newBroadcastTChan)
import Data.Time              (UTCTime, getCurrentTime)
import System.IO              (Handle)

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T

type UserName = T.Text
type RoomName = T.Text

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
                     , clientRoomChans :: TVar (Map.Map RoomName (TChan Message))
                     }

newClient :: User -> Handle -> IO Client
newClient user handle = do
  now             <- getCurrentTime
  clientPongTime  <- newMVar now
  clientChan      <- newTChanIO
  clientRoomChans <- newTVarIO Map.empty
  return $ Client user handle clientPongTime clientChan clientRoomChans

-- Private Room is used to group clients together so that they can complete in a match
data PrivateRoom = PrivateRoom { roomName   :: !RoomName
                               , roomUsers  :: TVar (Set.Set User)
                               , roomChan   :: TChan Message
                               }

newPrivateRoom :: RoomName -> Set.Set User -> STM PrivateRoom
newPrivateRoom roomName users = do
  roomUsers <- newTVar users
  roomChan  <- newBroadcastTChan
  return $ PrivateRoom roomName roomUsers roomChan

-- A server is a mutable map between User and client
data Server = Server { serverUsers :: MVar (Map.Map User Client) 
                     , serverRooms :: TVar (Map.Map RoomName PrivateRoom)
                     }

data Message = -- Server messages
               NameInUse UserName
             | LoggedIn UserName
             | Ping
             | MsgReply User T.Text
             | TellReply RoomName User T.Text
             | NoSuchUser UserName
             | Joined RoomName User
             | Leaved RoomName User
             | NamesReply RoomName (Set.Set User)
             | InvalidMessage T.Text
               -- Client messages
             | Pong
             | Login UserName
             | Msg User T.Text
             | Tell RoomName T.Text
             | Join RoomName
             | Leave RoomName
             | Names RoomName
             | Quit
               deriving (Show, Eq)
