module Game.Src.Haskscroller.HUD where

import HSCIIEngine.Display
import HSCIIEngine.Types
import HSCIIEngine.Objects

import Game.Src.Haskscroller.Areas
import qualified Game.Res.Palette as Clr

separatorBar
  = toObject img hudtl
  where
    img = fillSprite Clr.border $ ["╠" ++ (replicate (round vwidth) '═') ++ "╣"]

hpBarPos = hudtl + (V2 1 1)

cHealth = (RED, WHITE)
healthIcon = "█"
noHealthIcon = " "

createHPBar :: V2F -> Float -> Float -> Object
createHPBar pos hp maxHP
  = toObject img pos
  where
    img = fillSprite cHealth [concat icons]
    icons = (replicate hpInt healthIcon) ++ (replicate nhpInt noHealthIcon)
    hpInt = round hp
    nhpInt = round (maxHP - hp)
