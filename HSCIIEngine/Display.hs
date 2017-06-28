module HSCIIEngine.Display where

import HSCIIEngine.String
import HSCIIEngine.Types

type Side = Char
type Corner = Char
type Border = (Side, (Corner, Corner, Corner, Corner), Side)
lineBorder = ('║', ('╔', '╗', '╚', '╝'), '═')

--"\x1b[32m"

-- Terminal output

render :: Object -> IO()
render = (flip renderWithBorder) lineBorder

renderWithBorder :: Object -> Border -> IO()
renderWithBorder (_, _, img) customBorder
  = do
    mapM_ putStrLn (border customBorder img)
    return ()

wipe n
  = putStr ("\ESC[" ++ (show n) ++ "A")

hideCursor
  = putStr "\ESC[?25l"

showCursor
  = putStr "\ESC[?25h"

-- Borders

border :: Border -> Image -> Image
border (side, (ctl, ctr, cbl, cbr), top) img
  = [hbt] ++ (map (vborder side) img) ++ [hbb]
  where
    hbt = hborder (ctl, ctr, top) width
    hbb = hborder (cbl, cbr, top) width
    width = (length (head img))

hborder :: (Corner, Corner, Side) -> Width -> String
hborder (lcorner, rcorner, top) width
  = [lcorner] ++ (replicate width top) ++ [rcorner]

vborder :: Side -> String -> String
vborder side txt
  = [side] ++ txt ++ [side]
