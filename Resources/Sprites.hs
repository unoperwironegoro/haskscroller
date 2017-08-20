module Resources.Sprites where

import HSCIIEngine.Display
import HSCIIEngine.String

imgNull = plainCol $ artformat 0 ""

imgLogo = colourSprite (colourCode RED) $ artformatTransp 17
  ".__..__....______\n\
  \/ |.| |.../ ____|\n\
  \|_|_|_|..|_/____.\n\
  \(_____().(_____()\n\
  \| |.| |...____/ |\n\
  \|_|.|_/..|_____/." '.'

imgBind    = colourSprite (colourCode CYAN) $ artformat 3 ">>="
imgComment = colourSprite (colourCode WHITE) $ artformat 2 "--"
