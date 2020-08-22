{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game where

import Data.Time  (UTCTime, diffUTCTime)
import Lens.Micro
import Lens.Micro.TH

import qualified Data.Text  as T


newtype Quote = Quote { unQuote :: T.Text } deriving (Show)
newtype Input = Input { unInput :: T.Text } deriving (Show)

data State = State
  {
    _quote     :: Quote
  , _input     :: Input
  , _startTime :: Maybe UTCTime
  , _endTime   :: Maybe UTCTime
  , _hits      :: Int 
  , _strokes   :: Int
  } deriving (Show)

makeLenses ''State

-- | The character can either be hit correctly or missed
data Character = 
    Hit Char
  | Miss Char

-- | Take Quote and Input String and give back the list of character
-- | quote' are the characters from Quote that we match with the input string
character :: Quote -> Input -> [Character]
character (Quote quote) (Input input) = map mkChar $ T.zip quote' input
  where quote' = T.take (T.length input) quote
        mkChar (q,i)
          | q == i    = Hit i
          | otherwise = Miss i

isErrorFree :: State -> Bool
isErrorFree st = undefined

applyChar :: Char -> State -> State
applyChar char st = st & (strokes +~ 1) . (input %~ appendInput) . (hits +~ isErrorFree')
  where appendInput = Input <$> (T.cons char) . unInput
        isErrorFree' = if isErrorFree st then 1 else 0

test :: IO ()
test = putStrLn "Hello"
