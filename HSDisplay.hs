module HSDisplay where

import HSString
import HSTypes

type Border = (Char, Char, Char)
lineBorder = ('|', '+', '-')

-- Borders

border :: Dimensions -> Border -> [String] -> [String]
border (width, height) (side, corner, top) ls
  = [hb] ++ (map (vborder side) visls) ++ [hb]
  where
    hb = hborder (corner, top) width
    visls = (take height ls) ++ blanks
    blanks = replicate vpadheight (padblank width)
    vpadheight = (height - (length ls))

hborder :: (Char, Char) -> Width -> String
hborder (corner, top) width
  = [corner] ++ (pad width "" top) ++ [corner]

vborder :: Char -> String -> String
vborder _ [] = []
vborder side txt
  = [side] ++ txt ++ [side]
