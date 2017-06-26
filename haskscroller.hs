import HSString
import HSDisplay
import HSObjects
import HSText
import HSSprites
import HSIO

import Data.String
import Data.List
import Control.Monad

--------- Game config

gdim@(gwidth, gheight) = (80, 30)
gCanvas = blankCanvas gdim
fps = 15
mspf = (1000 / fps) :: Rational

--------- Game Flow & Logic

main = do
    calibrate
    splash
    return ()

---------- Calibration

objPrompt = toObject (txtformat (gwidth - 6) txtPrompt) (3,3)
objLogo = toObject (artformat 60 spriteLogo) (5,12)

calibrate = do
    render (drawOver gCanvas [objPrompt, objLogo])
    ready <- getLine
    return ()

---------- Splash screen

splash = do
  let frames = map ((drawOver gCanvas) . (:[])) (take 14 animLogoTrace)
  forM_ frames ((tdo mspf) . drawAnim)
  return ()

drawAnim objLogo = do
  render (drawOver gCanvas [objLogo])
  return ()

animLogoTrace = iterate ((flip move) (2, 1)) objLogo
