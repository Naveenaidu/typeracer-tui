{-# LANGUAGE OverloadedStrings #-}
module Game where

import Data.Time  (UTCTime, diffUTCTime)
import Data.Text

data State = State
  {
    quote     :: Text
  , input     :: Text
  , startTime :: Maybe UTCTime
  , endTime   :: Maybe UTCTime
  , hits      :: Int 
  , strokes   :: Int
  } deriving (Show)

-- | The character can either be hit correctly or missed
data Character = 
    Hit Char
  | Miss Char


test :: IO ()
test = putStrLn "Hello"
