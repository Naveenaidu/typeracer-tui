{- The code is highly motivated from https://github.com/callum-oakley/gotta-go-fast/ -}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where

import           Data.Word              (Word8)
import           Data.FileEmbed         (embedStringFile)
import           System.Console.CmdArgs (Data, Typeable, args, cmdArgs, def,
                                         details, help, name, program, summary,
                                         typ, (&=))
import           System.Random          (randomRIO)                                         

import UI
import Server.Server

data Config =
  Config
    { fg_empty          :: Maybe Word8
    , fg_error          :: Maybe Word8
    , fg_correct        :: Maybe Word8
    , nonsense_len      :: Int
    }
  deriving (Show, Data, Typeable)

config :: Config
config =
  Config
    { fg_empty =
        def &= typ "COLOUR" &=
        help "The ANSI colour code for empty (not yet typed) text"
    , fg_error = def &= typ "COLOUR" &= help "The ANSI colour code for errors"
    , fg_correct   = def &= typ "COLOUR" &= help "The ANSI colour code for correct hit"
    , nonsense_len =
        210 &= name "l" &= typ "WORDS" &=
        help "The length of nonsense to generate (default: 210)"
    } &=
  summary "TypeRacer TUI" &=
  help "Practice typing and measure your WPM and accuracy. And compete online with friends" &=
  program "typeracer-tui-exe"

-- wordWeights.txt is taken from
-- https://en.wiktionary.org/wiki/Wiktionary:Frequency_lists#TV_and_movie_scripts
-- (and cleaned up a little with some throwaway sed)
wordWeights :: [(String, Int)]
wordWeights =
  map ((\[w, f] -> (w, read f)) . words) . lines $
  $(embedStringFile "wordWeights.txt")

totalWeight :: Int
totalWeight = sum . map snd $ wordWeights

-- Pick a random word from wordWeights.txt 
weightedRandomWord :: IO String
weightedRandomWord = do
  r <- randomRIO (0, totalWeight - 1)
  return $ getWord r wordWeights
  where
    getWord r ((w, f):rest)
      | r < f = w
      | otherwise = getWord (r - f) rest

loopWhile :: Monad m => (a -> Bool) -> m a -> m a
loopWhile p mx = do
  x <- mx
  if p x
    then loopWhile p mx
    else return x

-- Generates nonsense which is superficially similar to English. Similar in the
-- sense that the frequency of words in the generated text is approximately the
-- same as the frequency of words in actual usage.
nonsense :: Config -> IO String
nonsense c = do
  words <- go (nonsense_len c) Nothing
  return $ ( unwords $ words) 
  where
    go n lastWord
      | n <= 0 = return []
      | otherwise = do
        word <- loopWhile ((== lastWord) . Just) weightedRandomWord
        rest <- go (n - length word - 1) (Just word) -- extra 1 to count the space
        return $ word : rest

main :: IO ()
main = do
  -- c <- cmdArgs config
  -- nonsense c >>= run (fg_empty c) (fg_error c) (fg_correct c)
  runServer "127.0.0.1" 9999
