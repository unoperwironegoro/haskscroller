module Scenes.Calibration where

import HSCIIEngine.Objects
import HSCIIEngine.Display
import HSCIIEngine.String
import HSCIIEngine.Types

import GameCommon

objPrompt = toObject (txtformat (gwidth - 6) txtPrompt) (V2 3 15)

calibrate
  = do
    resize gdim
    render (drawOver gCanvas [objPrompt, objLogo])
    ready <- getLine
    hideCursor
    return ()

txtPrompt
  = "Welcome to Haskscroller! Please keep the terminal size such that this \
  \box and the input line below is all that you can see. When ready press \
  \Enter to continue.\
  \ \n\
  \ \n\
  \WARNINGS: \n\
  \This game hides the cursor from your terminal! Please quit with ESC to \
  \ensure that you get it back! Efforts have been made to restore it when \
  \killed with Ctrl-C, but it does not always work! If you have lost your \
  \cursor, try running:\n\
  \ $ setterm -cursor on."
