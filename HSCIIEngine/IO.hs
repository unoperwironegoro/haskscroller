module HSCIIEngine.IO where

import HSCIIEngine.Display
import HSCIIEngine.Types

import System.IO
import System.Exit
import Control.Exception as E
import Control.Concurrent
import System.Posix.Signals

type Key = Char

takeKeys :: Handle -> IO ([Key])
takeKeys hnd = hReady hnd >>= f
   where f True = ((:) <$> getChar <*> (takeKeys hnd))
         f _    = return []

-- Stop requiring Enter for input, and prevent terminal echo
configureIO
  = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

-- Ctrl-C handler
installHandlers
  = do
    tid <- myThreadId
    installHandler keyboardSignal (Catch (catchCtrlC tid)) Nothing
    return ()

catchCtrlC tid
  = do
    showCursor
    throwTo tid UserInterrupt
