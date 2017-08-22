module Game.Src.Common where

import HSCIIEngine.Display
import HSCIIEngine.Types
import HSCIIEngine.Objects

import qualified Game.Res.Sprites as Spr
import qualified Game.Res.Palette as Clr

gdim@(V2 gwidth gheight) = (V2 100 30) :: Dimensions
fps = 30 :: Float
mspf = (1000 / (realToFrac fps)) :: Rational

secToFrames :: Float -> Int
secToFrames s = round (s * fps) 

gCanvas = canvasObject gdim [(colourCode2 Clr.plain, ' ')]
gBorder = wrap gCanvas whiteLineBorder

objLogo = toObject Spr.logo (V2 40 3)
