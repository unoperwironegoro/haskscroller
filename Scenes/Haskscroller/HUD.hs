module Scenes.Haskscroller.HUD where

import HSCIIEngine.Display
import HSCIIEngine.Types
import HSCIIEngine.Objects
import Scenes.Haskscroller.Areas

separatorBar
  = toObject img hudtl
  where
    img = plainCol $ ["╠" ++ (replicate (round vwidth) '═') ++ "╣"]

hpBarPos = hudtl + (V2 1 1)

healthIcon = "█"
noHealthIcon = " "

createHPBar :: V2F -> Float -> Float -> Object
createHPBar pos hp maxHP
  = toObject img pos
  where
    img = colourSprite (colourCode2 RED WHITE) [concat icons]
    icons = (replicate hpInt healthIcon) ++ (replicate nhpInt noHealthIcon)
    hpInt = round hp
    nhpInt = round (maxHP - hp)
