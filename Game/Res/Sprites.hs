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

eBind    = fillSprite Clr.ally       $ artformat ">>="
eComment = fillSprite Clr.background $ artformat "--"
