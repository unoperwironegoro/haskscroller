module Resources.Sprites where

import HSCIIEngine.String

imgNull = artformat 0 ""

imgLogo = artformatTransp 17
  ".__..__....______\n\
  \/ |.| |.../ ____|\n\
  \|_|_|_|..|_/____.\n\
  \(_____().(_____()\n\
  \| |.| |...____/ |\n\
  \|_|.|_/..|_____/."
  '.'

imgBind    = artformat 3 ">>="
imgComment = artformat 2 "--"
