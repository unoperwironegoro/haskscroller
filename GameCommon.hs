module GameCommon where

import HSCIIEngine.Display
import HSCIIEngine.Types
import HSCIIEngine.Objects

import Resources.Sprites

gdim@(V2 gwidth gheight) = (V2 100 30) :: Dimensions
fps = 30 :: Float
mspf = (1000 / (realToFrac fps)) :: Rational

gCanvas = blankCanvas gdim
gBorder = wrap gCanvas whiteLineBorder

objLogo = toObject imgLogo (V2 40 3)
