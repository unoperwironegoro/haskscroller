module HSCIIEngine.Display where

import HSCIIEngine.String
import HSCIIEngine.Types
import Data.List
import Data.Maybe

type Side a = a
type Corner a = a
type Border a = (a, (a, a, a, a), a)
lineBorder = ("║", ("╔", "╗", "╚", "╝"), "═")
colWhite = colourCode WHITE

data ANSIColour = GREY | RED | GREEN | YELLOW | BLUE |
                  MAGENTA | CYAN | WHITE | BLACK deriving (Eq)
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

-- Colouring
plainCol = colourSprite colWhite

colourSprite :: Colour -> Sprite -> Image
colourSprite colour sprite
  = map (map  (\c -> if c == alphaChar then alphaStr else (colour ++ [c]))) sprite

-- Terminal Adjustments
resize (V2 x y)
  = putStr ("\ESC[8;" ++ (show y') ++ ";" ++ (show x') ++ "t")
  where
    -- Taking into account border and input size
    y' = y + 2 + 1
    x' = x + 2

-- Terminal output
renderWithBorder :: Object -> Border String -> IO()
renderWithBorder (_, _, img) customBorder
  = do
    mapM_ putStrLn (mmc (borderCol customBorder colWhite img))
    return ()

render :: Object -> IO()
render (_, _, img)
  = do
    mapM_ putStrLn (mmc img)
    return ()

mmc :: Image -> [String]
mmc = map concat

wipe n
  = putStr ("\ESC[" ++ (show n) ++ "A")

hideCursor
  = putStr "\ESC[?25l"

showCursor
  = putStr "\ESC[?25h"

-- Borders
borderCol :: Border String -> Colour -> Image -> Image
borderCol (h, (c1, c2, c3, c4), v) c img
  = border b' img
  where
    b' = ((h ++ c), ((c1 ++ c), (c2 ++ c), (c3 ++ c), (c4 ++ c)), (v ++ c))

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
