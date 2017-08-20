module HSCIIEngine.Display where

import HSCIIEngine.String
import HSCIIEngine.Types
import Data.List
import Data.Maybe

type Side a = a
type Corner a = a
type Border a = (a, (a, a, a, a), a)

colWhite = colourCode WHITE

lineBorder = ('║', ('╔', '╗', '╚', '╝'), '═')
whiteLineBorder = colourBorder lineBorder [colWhite]

-- Colouring
plainCol = colourSprite colWhite

colourSprite :: ColourData -> Sprite -> Image
colourSprite cd sprite
  = map (map $ colourChar cd) sprite

colourChar :: ColourData -> Char -> Tile
colourChar cd c
  = (cd, c)

colourBorder :: Border Char -> [ColourData] -> Border Tile
colourBorder (h, (c1, c2, c3, c4), v) cds
  = ((k0, h), ((k1, c1), (k2, c2), (k3, c3), (k4, c4)), (k5, v))
  where
    [k0, k1, k2, k3, k4, k5] = take 6 (cycle cds)

data ANSIColour = GREY | RED | GREEN | YELLOW | BLUE |
                  MAGENTA | CYAN | WHITE | BLACK deriving (Eq)

fgTable = zip [GREY, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE]
              [30..37] :: [(ANSIColour, Int)]
bgTable = zip [GREY, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE, BLACK]
              ([40..47] ++ [49]) :: [(ANSIColour, Int)]

colourCode :: ANSIColour -> ColourData
colourCode fg
  = colourCode2 (fg, BLACK)

colourCode2 :: (ANSIColour, ANSIColour) -> ColourData
colourCode2 (fg, bg)
  = (fgi, bgi)
  where
    fgi = fromMaybe 39 (lookup fg fgTable)
    bgi = fromJust (lookup bg bgTable)

-- Terminal Adjustments
resize (V2 x y)
  = putStr ("\ESC[8;" ++ (show y') ++ ";" ++ (show x') ++ "t")
  where
    -- Taking into account border and input size
    y' = y + 2 + 1
    x' = x + 2

hideCursor
  = putStr "\ESC[?25l"

showCursor
  = putStr "\ESC[?25h"

-- Terminal output
render :: Object -> IO()
render (_, _, img)
  = do
    mapM_ putStrLn (convert img)
    return ()
  where
    -- Inserts minimum control characters
    convert :: Image -> [String]
    convert img = convert' colWhite img
    convert' :: ColourData -> [[Tile]] -> [String]
    convert' _ [] = []
    convert' lastcd (ts:tss)
      = line : (convert' lastcd' tss)
      where
        (line, lastcd') = convertLine lastcd ts

        convertLine :: ColourData -> [Tile] -> (String, ColourData)
        convertLine lcd [] = ([], lcd)
        convertLine lcd ((cd,c):ts)
          = (tileStr ++ recTileStr, endcd)
          where
          (recTileStr, endcd) = convertLine cd ts
          tileStr = (showC (diff cd lcd)) ++ [c]
          showC (Nothing, Nothing) = ""
          showC (Nothing, jci    ) = showC (jci, Nothing)
          showC (Just ci, Nothing) = "\ESC[" ++ (show ci) ++ "m"
          showC (Just fi, Just bi) = "\ESC[" ++ (show fi) ++
                                         ";" ++ (show bi) ++ "m"

    -- First ColourData has priority
    diff :: ColourData -> ColourData -> (Maybe Colour, Maybe Colour)
    diff (fg1, bg1) (fg2, bg2)
      = (diff' fg1 fg2, diff' bg1 bg2)
      where
        diff' def alt = if def == alt then Nothing else Just def

wipe n
  = putStr ("\ESC[" ++ (show n) ++ "A")

-- Borders
border :: Border a -> [[a]] -> [[a]]
border (side, (ctl, ctr, cbl, cbr), top) str
  = [hbt] ++ (map (vborder side) str) ++ [hbb]
  where
    hbt = hborder (ctl, ctr, top) width
    hbb = hborder (cbl, cbr, top) width
    width = (length (head str))

hborder :: (Corner a, Corner a , Side a) -> Width -> [a]
hborder (lcorner, rcorner, top) width
  = [lcorner] ++ (replicate width top) ++ [rcorner]

vborder :: Side a -> [a] -> [a]
vborder side txt
  = [side] ++ txt ++ [side]
