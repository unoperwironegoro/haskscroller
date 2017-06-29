module Scenes.Calibration where

import HSCIIEngine.Objects
import HSCIIEngine.Display
import HSCIIEngine.String
import HSCIIEngine.Types

import GameCommon

objPrompt = toObject (txtformat (gwidth - 6) txtPrompt) (V2 3 15)

calibrate
  = do
    render (drawOver gCanvas [objPrompt, objLogo])
    ready <- getLine
    hideCursor
    return ()

txtPrompt
  = "Welcome to Haskscroller! Before we begin the game, please adjust the terminal size until this box and the input line below is all that you can see. When finished press Enter to continue.\
  \ \n\
  \ \n\
  \WARNINGS: \n\
  \This game hides the cursor! Efforts have been made to restore it when killed with Ctrl-C, but it does not always work! Simply restart the game and terminate it again if this happens."
