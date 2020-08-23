{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game  where

import Data.Time  (UTCTime, diffUTCTime)
import Lens.Micro
import Lens.Micro.TH

import qualified Data.Text  as T


newtype Quote = Quote { unQuote :: T.Text } deriving (Show)
newtype Input = Input { unInput :: T.Text } deriving (Show)

data Game = Game
  {
    _quote     :: Quote
  , _input     :: Input
  , _startTime :: Maybe UTCTime
  , _endTime   :: Maybe UTCTime
  , _hits      :: Int 
  , _strokes   :: Int
  } deriving (Show)

makeLenses ''Game

-- | The character can either be hit correctly or missed
data Character = 
    Hit Char
  | Miss Char

type Line = [Character]

-- | Take Quote and Input String and give back the list of character
-- | quote' are the characters from Quote that we match with the input string
character :: Quote -> Input -> [Character]
character (Quote quote) (Input input) = map mkChar $ T.zip quote' input
  where quote' = T.take (T.length input) quote
        mkChar (q,i)
          | q == i    = Hit i
          | otherwise = Miss i

-- | Check if the Input at time `t` is equal to that much part of quote
isErrorFree :: Game -> Bool
isErrorFree st = input' `T.isPrefixOf` quote'
  where quote' = st^.quote & unQuote
        input'  = st^.input & unInput

-- | The game is complete when input is equal to quote
isComplete :: Game -> Bool
isComplete st = quote' == input'
  where quote' = st^.quote & unQuote
        input'  = st^.input & unInput

applyChar :: Char -> Game -> Game
applyChar char st = st & (strokes +~ 1) . (input %~ appendInput) . (hits +~ isErrorFree')
  where appendInput = Input <$> (T.cons char) . unInput
        isErrorFree' = if isErrorFree st then 1 else 0

applyBackSpace :: Game -> Game
applyBackSpace =  undefined

applyBackSpaceWord :: Game -> Game
applyBackSpaceWord = undefined

applyWhiteSpace :: Game -> Game
applyWhiteSpace = undefined

test :: IO ()
test = putStrLn "Hello"
