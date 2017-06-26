module HSDisplay where

import HSString
import HSTypes

type Side = Char
type Corner = Char
type Border = (Side, (Corner, Corner, Corner, Corner), Side)
lineBorder = ('║', ('╔', '╗', '╚', '╝'), '═')

render :: Object -> IO()
render (_, _, img) = do
    mapM_ putStrLn (border lineBorder img)
    return ()


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
