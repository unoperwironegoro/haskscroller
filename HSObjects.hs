module HSObjects where

import HSString
import HSTypes

toObject :: [String] -> Coords -> Object
toObject ls coords
  = (coords, dim, ls)
  where
    dim = (width, height)
    width = length (head ls)
    height = length ls

renderObjects :: [Object] -> [Object] -> [Object]
renderObjects objects canvas
  = undefined

blankCanvasObject width height
  = ((0, 0), (width, height), blanks)
  where
    blanks = replicate height (padblank width)
