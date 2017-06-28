module GameCommon where

import HSCIIEngine.Display
import HSCIIEngine.Types
import HSCIIEngine.Objects

import Resources.Sprites

gdim@(gwidth, gheight) = (100, 30) :: Dimensions
fps = 30 :: Float
mspf = (1000 / (realToFrac fps)) :: Rational

gCanvas = blankCanvas gdim

objLogo = toObject imgLogo (40,3)
