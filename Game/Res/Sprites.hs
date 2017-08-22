module Game.Res.Sprites where

import HSCIIEngine.Display
import HSCIIEngine.String

import qualified Game.Res.Palette as Clr

blank = fillSprite Clr.plain $ artformat ""

logo = fillSprite Clr.logo $ artformatTransp
  ".__..__....______\n\
  \/ |.| |.../ ____|\n\
  \|_|_|_|..|_/____.\n\
  \(_____().(_____()\n\
  \| |.| |...____/ |\n\
  \|_|.|_/..|_____/." '.'

-- Entities

ePlayer     = fillSprite Clr.ally       $ artformat "repeat (-)"
eComment    = fillSprite Clr.background $ artformat "--"
eProjectile = fillSprite Clr.ally       $ artformat "-"

eConst      = fillSprite Clr.enemy      $ artformat "const --"
