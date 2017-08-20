module Scenes.Haskscroller.HUD where

import HSCIIEngine.Display
import HSCIIEngine.Types
import HSCIIEngine.Objects
import Scenes.Haskscroller.Areas

separatorBar = toObject [replicate (round vwidth) '='] hudtl

hpBarPos = V2 0 vheight

healthIcon = '♥'
noHealthIcon = '-'

createHPBar :: V2F -> Float -> Float -> Object
createHPBar pos hp maxHP
  = toObject img pos
  where
    img = border lineBorder bar
    bar = [(replicate hpInt healthIcon) ++ (replicate nhpInt noHealthIcon)]
    hpInt = round hp
    nhpInt = round (maxHP - hp)
