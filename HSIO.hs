module HSIO where

import System.IO
import Control.Concurrent
import Data.Time.Clock

-- TODO understand how this works
ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

tdo time body = do
     start <- getCurrentTime
     body
     end <- getCurrentTime
     let diff = diffUTCTime end start
         usecs = toRational diff * 1000000
         delay = time*1000 - usecs
     if delay > 0
       then threadDelay (floor delay)
       else return ()
