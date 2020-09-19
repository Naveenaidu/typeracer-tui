module Server.Client where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception      hiding (handle)
import Control.Monad          (void, forever, when, unless, forM_)
import Data.Time              (getCurrentTime, diffUTCTime)
import System.IO              (hGetLine, Handle)
import System.IO.Error        (isEOFError)
import System.Timeout         (timeout)
import Text.Printf            (printf, hPrintf)

printToHandle :: Handle -> String -> IO ()
printToHandle handle = hPrintf handle "%s\n"
