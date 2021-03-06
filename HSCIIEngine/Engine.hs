module HSCIIEngine.Engine
  -- TODO Turn into a package!
  (runGame, gloop, tgloop,
   module HSCIIEngine.Objects,
   module HSCIIEngine.String,
   module HSCIIEngine.Types,
   module HSCIIEngine.Display) where

import HSCIIEngine.String
import HSCIIEngine.Display
import HSCIIEngine.Objects
import HSCIIEngine.IO
import HSCIIEngine.Types

import Data.String
import Data.List
import Data.Maybe
import Control.Monad
import Control.Concurrent
import Data.Time.Clock
import System.IO

runGame scenes
  = do
    installHandlers
    configureIO
    sequence scenes
    showCursor
    -- TODO clear screen after finish?
    return ()

-- Game loop
gloop = tgloop (-1)

-- TODO pass around debugging info (framerate)
tgloop :: Int                    -> -- Max Cycles (< 0 := inf)
          Rational               -> -- Loop time /ms
          (w -> IO())            -> -- Display function
          ([Key] -> a)           -> -- Key interpreter
          (w -> a -> Maybe w)    -> -- Game logic
          Maybe w                -> -- Initial world state
          IO()
tgloop mcycles tms output keyintr logic world
  = do
    now <- getCurrentTime
    gloop' mcycles world now
  where
    gloop' _ Nothing _
      = return()
    gloop' 0 _ _
      = return()
    gloop' cycles (Just world) t
      = do
        output world
        input <- (takeKeys stdin)
        let cycles' = cycles - 1
            intr = (keyintr input)
            world' = logic world intr
        t' <- getCurrentTime
        let diff = diffUTCTime t' t
            usecs = toRational diff * 1000000
            delay = tms*1000 - usecs
        when (delay > 0) (threadDelay (floor delay))
        gloop' cycles' world' t'
