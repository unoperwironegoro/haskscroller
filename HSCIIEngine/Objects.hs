module HSCIIEngine.Objects where

import HSCIIEngine.String
import HSCIIEngine.Types

toObject :: [String] -> Coords -> Object
toObject img coords
  = (coords, dim, img)
  where
    dim = (width, height)
    width = length (head img)
    height = length img

move :: Float -> Object -> Coords -> Object
move scale ((x, y), dim, img) (dx, dy)
  = ((x + dx', y + dy'), dim, img)
  where
    dx' = dx / scale
    dy' = dy / scale

drawOver :: Object -> [Object] -> Object
drawOver canvas [] = canvas
drawOver (coords, (mw, mh), base) (((x,y), (w, h), img):objs)
  = drawOver (coords, (mw, mh), canvas') objs
  where
    canvas' = drawOver' y' h mh canvas'' base
    canvas'' = zipWith (drawOver' x' w mw) img drawnRows
    drawnRows = take h (drop y' base)
    x' = round x
    y' = round y

drawOver' :: Int -> Int -> Int -> [a] -> [a] -> [a]
drawOver' offset n maxsize mask base
  = take maxsize (pre ++ mask ++ post)
  where
    pre = take offset base
    post = drop (n + offset) base

canvasObject :: Dimensions -> String -> Object
canvasObject (width, height) pattern
  = ((0, 0), (width, height), image)
  where
    image = replicate height rows
    rows = take width (repeatstr pattern)

blankCanvas = (flip canvasObject) " "
