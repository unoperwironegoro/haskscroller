module HSCIIEngine.String where

import HSCIIEngine.Types
import Data.Char

-- Used to represent transparency
alphaChar = chr 30 :: Char

pad :: Char -> Width -> String -> String
pad ch w str = take w (str ++ (repeat ch))

padspace :: Width -> String -> String
padspace = pad ' '

padalpha :: Width -> String -> String
padalpha = pad alphaChar

------------------- Art
-- | Converts a \n-separated ASCII-art string into a sprite, making all
-- | occurrences of alphaCh transparent.
artformatTransp :: String -> Char -> Sprite
artformatTransp txt alphaCh
  = map ((padalpha width) . (map repl)) txtLines
  where
    repl ch | ch == alphaCh = alphaChar
            | otherwise     = ch
    txtLines = lines txt
    width = maximum $ map length txtLines

-- | Converts a \n-separated ASCII-art string into an opaque sprite.
artformat :: String -> Sprite
artformat txt = artformatTransp txt alphaChar

------------------- Text handling

-- | Converts a passage into an opaque sprite, with bounded by a text box of
-- | size width, height. Excess characters are truncated.
-- TODO handle empty newlines
txtformat :: Dimensions -> String -> Sprite
txtformat (V2 width height) txt
  = map (padspace width) ls
  where
    paragraphs = lines txt
    ls = concatMap (overflow width) (map emptytospace wordsList)
    wordsList = (map words paragraphs)

--TODO personal codes ???
emptytospace :: [String] -> [String]
emptytospace [] = [""]
emptytospace ws = ws

overflow :: Int -> [String] -> [String]
overflow _ [] = []
overflow width (w:ws)
  = if wlen > width
      then whead : (overflow width (wtail:ws))
      else (w ++ l) : (overflow width rws)
  where
    wlen = length w
    limit = width - wlen
    (l, rws) = buildLine ws limit

    (whead, wtail) = splitAt width w

    buildLine :: [String] -> Int -> (String, [String])
    buildLine [] _ = ([], [])
    buildLine (w:ws) lim
      = if lim' < 0
          then if wl > width
                 then (' ' : wh, (wt:ws))
                 else ([], (w:ws))
          else ((' ' : (w ++ l)), ls)
      where
        wl = length w
        lim' = lim - (1 + wl)
        (l, ls) = buildLine ws lim'

        (wh, wt) = splitAt (lim - 1) w
