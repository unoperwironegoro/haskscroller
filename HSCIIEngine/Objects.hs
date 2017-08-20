module HSCIIEngine.Objects where

import HSCIIEngine.String
import HSCIIEngine.Types
import HSCIIEngine.Display

import Data.List

toObject :: Image -> V2F -> Object
toObject img coords
  = (coords, dim, img)
  where
    dim = V2 width height
    width = length (head img)
    height = length img

move :: Float -> Object -> V2F -> Object
move scale (pos, dim, img) disp
  = ((pos + disp'), dim, img)
  where
    disp' = (fmap (/ scale) disp)

drawOver :: Object -> [Object] -> Object
drawOver canvas [] = canvas
drawOver (coords, cdim, base) ((pos, (V2 w h), img):objs)
  = drawOver (coords, cdim, canvas') objs
  where
    (V2 mw mh) = cdim
    (V2 x' y') = fmap (round) pos
    -- Split the canvas, making a burger around the drawn rows
    (top, midrow, bottom) = splitAt2 y' h base
    canvas' = reassemble mh top burger bottom
    -- Split the burger, making another burger around columns
    burger' = map (splitAt2 x' w) midrow
    burger = zipWith draw burger' img

    draw (left, mid, right) row
      = reassemble mw left mid' right
      where
        mid' = zipWith overWrite mid row
        overWrite :: Tile -> Tile -> Tile
        overWrite oldTile newTile
          | oldTile == alphaStr = oldTile
          | otherwise           = newTile

    reassemble :: Int -> [a] -> [a] -> [a] -> [a]
    reassemble maxsize pre mid post
      = take maxsize (pre ++ mid ++ post)

    splitAt2 :: Int -> Int -> [a] -> ([a], [a], [a])
    splitAt2 offset len list
      | offset > 0 = (pre, middle, post)
      | otherwise  = (pre', middle', post')
      where
        (pre, rest) = splitAt offset list
        (middle, post) = splitAt len rest
        (rest', post') = splitAt (offset + len) list
        (pre', middle') = splitAt offset rest'


canvasObject :: Dimensions -> [Tile] -> Object
canvasObject dim@(V2 width height) pattern
  = (v2fzero, dim, image)
  where
    image = replicate height rows
    rows = take width ((concat . repeat) pattern)

wrap :: Object -> Border String -> Colour -> Object
wrap (coords, dim, img) customBorder colour
  = (coords, dim + (V2 2 2), borderCol customBorder colour img)

blankCanvas = (flip canvasObject) [" "]
