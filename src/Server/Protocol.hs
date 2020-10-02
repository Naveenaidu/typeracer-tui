module Server.Protocol where

import Text.Printf (printf)

import qualified Data.Set as Set

import Server.Types

parseCommand :: String -> Maybe Message
parseCommand command = case words command of
--   ["PONG"]                   -> Just Pong
  -- "LOGIN" : userName         -> Just $ Login (unwords userName)
--   ["QUIT"]                   -> Just Quit
  "CREATEROOM" : userName : maxUsers  -> Just $ CreateRoom userName (read (unwords maxUsers) :: Int)
  "JOINROOM" : roomName : userName    -> Just $ JoinRoom (roomName) (unwords userName)
--   "LEAVE" : roomName         -> Just $ Leave (unwords roomName)
--   "NAMES" : roomName         -> Just $ Names (unwords roomName)
  _                          -> Nothing
--   where
--     createRoomMsg = "CREATEROOM" : userName : maxUsers

formatMessage :: Message -> String
formatMessage (NameInUse name) = printf "NAME IN USE %s" name 
formatMessage (RoomNameInUse name) = printf "ROOM NAME IN USE %s" name 
formatMessage (RoomNotExist name) = printf "ROOM DOES NOT EXIST %s" name 
formatMessage (RoomFull name) = printf "ROOM IS FULL %s" name 
formatMessage (RoomCreated name) = printf "ROOM CREATED %s" name
formatMessage (JoinedRoom roomName userName) = printf "%s JOINED ROOM %s" userName roomName
formatMessage (MatchStart roomName) = printf "MATCH STARTED IN ROOM %s" roomName
formatMessage (SendScoreStart roomName maxUsers) = printf "SENDING SCORES TO CLIENT IN ROOM %s" roomName
formatMessage (SendScore roomName userName rank score) =
  printf "%s SCORED %s accuracy and %s wpm in room %s with rank %s" 
    userName (accuracy score) (wpm score) roomName rank
formatMessage (SendScoreEnd roomName maxUsers) = 
  printf "%s SCORES SENT TO CLIENsT IN ROOM %s"  maxUsers roomName
