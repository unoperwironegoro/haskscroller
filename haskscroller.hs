import HSString
import HSDisplay
import HSObjects
import HSText
import HSSprites
import HSIO
import HSTypes

import Data.String
import Data.List
import Control.Monad
import Control.Concurrent
import Data.Time.Clock

--------- Game config

gdim@(gwidth, gheight) = (80, 30)
gCanvas = blankCanvas gdim
fps = 30
mspf = (1000 / fps) :: Rational

--------- Game Flow & Logic

main
  = do
    installHandlers
    calibrate
    tloop mspf 50 renderSplash splash objLogo
    gameloop
    showCursor
    return ()

gameloop
  = do
    return ()

-- Timed iterateM
tloop tms cycles output effector world
  = do
    t <- getCurrentTime
    tloop' tms cycles output effector world t

tloop' tms cycles output effector world t
  = do
    wipe (gheight + 3) -- 2 + 1 (border, input line)
    output world
    let cycles' = cycles - 1
        world' = effector world
    t' <- getCurrentTime
    let diff = diffUTCTime t' t
        usecs = toRational diff * 1000000
        delay = tms*1000 - usecs
    if delay > 0
      then threadDelay (floor delay)
      else return ()
    if cycles == 0
      then return ()
      else tloop' tms cycles' output effector world' t'

---------- Calibration

objPrompt = toObject (txtformat (gwidth - 6) txtPrompt) (3,15)
objLogo = toObject (artformat 60 spriteLogo) (30,3)

calibrate
  = do
    render (drawOver gCanvas [objPrompt, objLogo])
    ready <- getLine
    hideCursor
    return ()

---------- Splash screen

splash :: Object -> Object
splash
  = (flip move) (0, 0.6)

renderSplash obj
  = do
    render (drawOver gCanvas [obj])
    return ()
