{-
The code is highly motivated from 
https://github.com/callum-oakley/gotta-go-fast/blob/master/src/GottaGoFast.hs
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game  where

import    Data.Time       (UTCTime, diffUTCTime)
import    Lens.Micro
import    Lens.Micro.TH
import    Data.Char       (isSpace)
import    Data.Maybe

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
    , _input = Input (T.pack "")
    , _startTime = Nothing
    , _endTime = Nothing
    , _strokes = 0
    , _hits = 0
    }

-- | The character can either be hit correctly or missed
data Character = 
    Hit Char
  | Miss Char
  | Empty Char
    deriving (Show)

type Line = [Character]

startClock :: UTCTime -> Game -> Game
startClock now game = game & (startTime .~ Just now)

stopClock :: UTCTime -> Game -> Game
stopClock now game = game & (endTime .~ Just now)

hasStarted :: Game -> Bool
hasStarted game = isJust $ game ^. startTime

hasEnded :: Game -> Bool
hasEnded game = isJust $ game ^. endTime


-- | Cursor Column
-- | Divide the charcters in matrix n * 80, that'll tell us the position
cursorCol :: Input -> Int
cursorCol input 
  | lenInput `mod` 80 == 0 = 80
  | otherwise = lenInput `mod` 80
  where input' = unInput input
        lenInput = T.length input'


-- | Cursor Row
-- | Each line consits of 80 chars. Divide the chars into chunks of 80 and then get the length of list
cursorRow :: Input -> Int
cursorRow  input' = length (T.chunksOf 80  $ unInput input') -  1

cursor :: Game -> (Int, Int)
cursor g = (cursorCol input', cursorRow input')
  where input' = g^.input 

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

-- | Take Quote and Input String and give back the list of character
-- | quote' are the characters from Quote that we match with the input string
character :: Quote -> Input -> [Character]
character (Quote quote) (Input input) = (map mkChar (T.zip quote' input)) ++ (map (\c -> Empty c) remElems)
  where lenInput = (T.length input)
        lenQuote = (T.length quote)
        quote' = T.take lenInput  quote
        remElems  = [  T.index quote i  | i <- [lenInput, lenInput+1 .. lenQuote-1] ]
        mkChar (q,i)
          | q == i    = Hit i
          | q == ' ' && q /= i = Miss ' '
          | otherwise = Miss q

-- | Apply a char to the Game state
-- TODO: Instead of settings the `input` using `%~` to modify the input
applyChar :: Char -> Game -> Game
applyChar char st = st & (strokes +~ 1) . (input .~ newInput) . (hits +~ isErrorFree')
  where newInput = Input $ T.snoc (unInput (st ^. input)) char
        isErrorFree' = if isErrorFree st then 1 else 0

-- | Delete a single char from the input string
applyBackSpace :: Game -> Game
applyBackSpace game = game & (input .~ newInput)
  where newInput = Input $ T.reverse . T.drop 1 . T.reverse $ unInput $ game ^. input   

applyBackSpaceWord :: Game -> Game
applyBackSpaceWord = undefined

applyWhiteSpace :: Game -> Game
applyWhiteSpace = undefined

-- The following functions are only safe to use when both hasStarted and
-- hasEnded hold.
seconds :: Game -> Rational
seconds game = toRational $ diffUTCTime (fromJust end) (fromJust start)
  where start = game ^. startTime
        end   = game ^. endTime

countChars :: Game -> Int
countChars game = T.length $ unQuote (game^.quote)

wpm :: Game -> Rational
wpm game = fromIntegral (countChars game) / (5 * seconds game / 60) 

accuracy :: Game -> Rational
accuracy game = fromIntegral (game ^. hits) / fromIntegral (game^.strokes)

