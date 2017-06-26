module HSObjects where

import HSString
import HSTypes

toObject :: [String] -> Coords -> Object
toObject img coords
  = (coords, dim, img)
  where
    dim = (width, height)
    width = length (head img)
    height = length img

move :: Object -> Coords -> Object
move ((x, y), dim, img) (dx, dy)
  = ((x + dx, y + dy), dim, img)

drawOver :: Object -> [Object] -> Object
drawOver canvas [] = canvas
drawOver (coords, (mw, mh), base) (((x,y), (w, h), img):objs)
  = drawOver (coords, (mw, mh), canvas') objs
  where
    canvas' = drawOver' y h mh canvas'' base
    canvas'' = zipWith (drawOver' x w mw) img drawnRows
    drawnRows = take h (drop y base)

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
