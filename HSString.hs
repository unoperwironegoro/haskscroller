module HSString where

import HSTypes

-- String Formatting

pad :: Width -> String -> Char -> String
pad width txt ch
  = take width (txt ++ (repeat ch))
padspace width txt
  = pad width txt ' '
padblank width
  = pad width "" ' '

artformat :: Width -> String -> [String]
artformat width txt
  = map (padspace width) (lines txt)

txtformat :: Width -> String -> [String]
txtformat width txt
  = map (padspace width) ls
  where
    paragraphs = lines txt
    ls = concatMap (overflow width) (map words paragraphs)

-- Assume no word with length > gwidth
overflow :: Int -> [String] -> [String]
overflow _ [] = []
overflow width (w:ws)
  = (w ++ l) : (overflow width rws)
  where
    limit = width - (length w)
    (l, rws) = buildLine ws limit
-- Aux
buildLine :: [String] -> Int -> (String, [String])
buildLine [] _ = ([], [])
buildLine (w:ws) limit
  = if limit' < 0
    then ([], (w:ws))
    else ((' ' : (w ++ l)), ls)
  where
    limit' = limit - (1 + (length w))
    (l, ls) = buildLine ws limit'
