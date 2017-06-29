module HSCIIEngine.String where

import HSCIIEngine.Types

import Data.Char

-- Used to represent transparency
alphaChar = chr 30 :: Char

repeatstr :: String -> String
repeatstr str = [1..] >> str

pad :: Char -> Width -> String -> String
pad ch w str = take w (str ++ (repeat ch))

padspace :: Width -> String -> String
padspace = pad ' '

padalpha :: Width -> String -> String
padalpha = pad alphaChar

------------------- Art

artformatTransp :: Width -> String -> Char -> [String]
artformatTransp width txt alphaCh
  = map ((padalpha width) . (map repl))(lines txt)
  where
    repl ch | ch == alphaCh = alphaChar
            | otherwise     = ch

artformat :: Width -> String -> [String]
artformat width txt = artformatTransp width txt alphaChar

------------------- Text handling

-- TODO handle empty newlines
txtformat :: Width -> String -> [String]
txtformat width txt
  = map (padspace width) ls
  where
    paragraphs = lines txt
    ls = concatMap (overflow width) (map emptytospace wordsList)
    wordsList = (map words paragraphs)

--TODO personal codes
emptytospace :: [String] -> [String]
emptytospace [] = [""]
emptytospace ws = ws

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
