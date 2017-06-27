module HSIO where

import HSDisplay

import System.IO
import System.Exit
import Control.Exception as E
import Control.Concurrent
import System.Posix.Signals

-- TODO understand how this works
ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

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
