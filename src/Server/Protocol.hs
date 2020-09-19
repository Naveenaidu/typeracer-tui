module Server.Protocol where

import Text.Printf (printf)
import qualified Data.Set as Set
import Server.Types

-- parseCommand :: String -> Maybe Message
-- parseCommand command = case words command of
--   ["PONG"]                   -> Just Pong
--   "LOGIN" : userName         -> Just $ Login (unwords userName)
--   ["QUIT"]                   -> Just Quit
--   "CREATEROOM" : userName : maxUsers  -> Just $ CreateRoom
--   "JOIN" : roomName          -> Just $ Join (unwords roomName)
--   "LEAVE" : roomName         -> Just $ Leave (unwords roomName)
--   "NAMES" : roomName         -> Just $ Names (unwords roomName)
--   _                          -> Nothing
--   where
--     createRoomMsg = "CREATEROOM" : userName : maxUsers
