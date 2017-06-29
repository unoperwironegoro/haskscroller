module HSCIIEngine.Objects where

import HSCIIEngine.String
import HSCIIEngine.Types

import Data.List

toObject :: Image -> Coords -> Object
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
    x' = round x
    y' = round y
    -- Split the canvas, making a burger around the drawn rows
    (top, midrow, bottom) = splitAt2 y' h base
    canvas' = reassemble mh top burger bottom
    -- Split the burger, making another burger around columns
    burger' = map (splitAt2 x' w) midrow
    burger = zipWith draw burger' img

    draw (left, mid, right) row
      = reassemble mw left mid' right
      where
        mid' = zipWith drawTransp mid row
        drawTransp b i | i == alphaChar = b
                       | otherwise      = i

    reassemble :: Int -> [a] -> [a] -> [a] -> [a]
    reassemble maxsize pre mid post
      = take maxsize (pre ++ mid ++ post)

    splitAt2 :: Int -> Int -> [a] -> ([a], [a], [a])
    splitAt2 offset len list
      = (pre, middle, post)
      where
        (pre, rest) = splitAt offset list
        (middle, post) = splitAt len rest


canvasObject :: Dimensions -> String -> Object
canvasObject (width, height) pattern
  = ((0, 0), (width, height), image)
  where
    image = replicate height rows
    rows = take width (repeatstr pattern)

blankCanvas = (flip canvasObject) " "
