module HSCIIEngine.Display where

import HSCIIEngine.String
import HSCIIEngine.Types
import Data.List
import Data.Maybe

type Side a = a
type Corner a = a
type Border a = (a, (a, a, a, a), a)

data ANSIColour = GREY | RED | GREEN | YELLOW | BLUE |
                  MAGENTA | CYAN | WHITE | BLACK deriving (Eq)

colWhite = colourCode WHITE

lineBorder = ("║", ("╔", "╗", "╚", "╝"), "═")
whiteLineBorder = colourBorder lineBorder [colWhite]

-- Colouring
plainCol = colourSprite colWhite

colourSprite :: Colour -> Sprite -> Image
colourSprite colour sprite
  = map (map $ colourChar colour) sprite

colourChar :: Colour -> Char -> Tile
colourChar colour c
  = if c == alphaChar
      then alphaTile
      else colour ++ [c]

colourBorder :: Border String -> [Colour] -> Border String
colourBorder (h, (c1, c2, c3, c4), v) colours
  = ((k0 ++ h), ((k1 ++ c1), (k2 ++ c2), (k3 ++ c3), (k4 ++ c4)), (k5 ++ v))
  where
    [k0, k1, k2, k3, k4, k5] = take 6 (cycle colours)

fgTable = zip [GREY, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE]
              [30..37] :: [(ANSIColour, Int)]
bgTable = zip [GREY, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE, BLACK]
              ([40..47] ++ [49]) :: [(ANSIColour, Int)]

colourCode :: ANSIColour -> Colour
colourCode fg
  = colourCode2 fg BLACK

colourCode2 :: ANSIColour -> ANSIColour -> Colour
colourCode2 fg bg
  = "\ESC[" ++ fgInt ++ ";" ++ bgInt ++ "m"
  where
    fgInt = show $ fromMaybe 39 (lookup fg fgTable)
    bgInt = show $ fromJust (lookup bg bgTable)

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
    mapM_ putStrLn output
    return ()
  where
    output = map (concatMap simplify) img
    simplify = id
    -- Eliminates unnecessary control characters
    -- simplify :: [Tile] -> Image
    -- simplify ts
    --   = simplify' Nothing ts
    --   where
    --     simplify' :: Maybe Colour -> [Tile] -> Image
    --     simplify' Nothing ((col,c):ts)
    --       = s : (simplify' (colour s) ss)
    --     simplify' (Maybe c) (s:ss)
    --       = s' :
    --       where
    --         s'
    --     strip ()

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
