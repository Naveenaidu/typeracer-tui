{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game  where

import Data.Time  (UTCTime, diffUTCTime)
import Lens.Micro
import Lens.Micro.TH
import           Data.Char  (isSpace)

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

initialState :: String -> Game
initialState t =
  Game
    { _quote = Quote (T.pack t)
    , _input = Input (T.pack " ")
    , _startTime = Nothing
    , _endTime = Nothing
    , _strokes = 0
    , _hits = 0
    }

-- | The character can either be hit correctly or missed
data Character = 
    Hit Char
  | Miss Char
    deriving (Show)

type Line = [Character]

-- | Take Quote and Input String and give back the list of character
-- | quote' are the characters from Quote that we match with the input string
-- | QUESTION: Do we really want to allow chars more than `quote` length?? 
character :: Quote -> Input -> [Character]
character (Quote quote) (Input input) = (map mkChar (T.zip quote' input)) ++ (map (\c -> Miss c) remElems)
  where lenInput = (T.length input)
        lenQuote = (T.length quote)
        quote' = T.take lenInput  quote
        remElems  = [  T.index input i  | i <- [lenQuote+1, lenQuote+2 .. lenInput-1] ]
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

-- | TODO: use appendInput instead of newInput
applyChar :: Char -> Game -> Game
applyChar char st = st & (strokes +~ 1) . (input .~ newInput) . (hits +~ isErrorFree')
  where appendInput = Input <$>  (T.cons char) . unInput
        newInput = Input $ T.snoc (unInput (st ^. input)) char
        isErrorFree' = if isErrorFree st then 1 else 0

applyBackSpace :: Game -> Game
applyBackSpace =  undefined

applyBackSpaceWord :: Game -> Game
applyBackSpaceWord = undefined

applyWhiteSpace :: Game -> Game
applyWhiteSpace = undefined

-- | TODO: Change it when we shift the typebox to have word wrap
cursorCol :: Input -> Int
cursorCol  = T.length . T.takeWhile (/= '\n') . T.reverse . unInput 

cursorRow :: Input -> Int
cursorRow = T.length . T.filter (== '\n') . unInput

cursor :: Game -> (Int, Int)
cursor g = (cursorCol input', cursorRow input')
  where input' = g^.input 
  

test :: IO ()
test = putStrLn "Hello"
