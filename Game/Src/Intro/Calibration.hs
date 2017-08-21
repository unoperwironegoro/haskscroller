module Game.Src.Intro.Calibration where

import HSCIIEngine.Objects
import HSCIIEngine.Display
import HSCIIEngine.String
import HSCIIEngine.Types

import Game.Src.Common
import qualified Game.Res.Text as Txt
import qualified Game.Res.Palette as Clr

calibrate
  = do
    resize gdim
    render (emborder gBorder (drawOver gCanvas [objPrompt, objLogo]))
    ready <- getLine
    hideCursor
    return ()

objPrompt
  = toObject txtSpr (V2 (fromIntegral margin) 15)
  where
    txtSpr = fillSprite Clr.plain (txtformat dim Txt.welcome)
    margin = 3
    dim = V2 (gwidth - (2 * margin)) 15
