module HSCIIEngine.Engine where

import HSCIIEngine.String
import HSCIIEngine.Display
import HSCIIEngine.Objects
import HSCIIEngine.IO
import HSCIIEngine.Types

import Data.String
import Data.List
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
    return ()

-- Game loop
gloop :: Rational               -> -- Loop time /ms
         (w -> IO())            -> -- Display function
         ([Key] -> a)           -> -- Key interpreter
         (w -> a -> w)          -> -- Game logic
         w                      -> -- Initial world state
         (w -> Bool)            -> -- Terminal state evaluator
         IO()
gloop = tgloop 0

tgloop :: Int                    -> -- Max Cycles (< 1 := inf)
          Rational               -> -- Loop time /ms
          (w -> IO())            -> -- Display function
          ([Key] -> a)           -> -- Key interpreter
          (w -> a -> w)          -> -- Game logic
          w                      -> -- Initial world state
          (w -> Bool)            -> -- Terminal state evaluator
          IO()
tgloop cycles tms output keyintr logic world fin
  = do
    t <- getCurrentTime
    gloop' (tms, output, keyintr, logic, fin) cycles world t

-- Aux
gloop' consts@(tms, output, keyintr, logic, fin) cycles world t
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
    if ((fin world) || (cycles == 1))
      then return ()
      else gloop' consts cycles' world' t'
