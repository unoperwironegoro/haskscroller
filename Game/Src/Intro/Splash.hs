module Game.Src.Intro.Splash where

import HSCIIEngine.Engine
import HSCIIEngine.Objects
import HSCIIEngine.Display
import HSCIIEngine.Types

import Game.Src.Common

import Data.Maybe

type Splash = [Object]

splash = tgloop 70 mspf draw anyKey update initState

initState = Just [objLogo]

update :: Splash -> Bool -> Maybe Splash
update state False
  = Just (map ((flip (move fps)) (V2 0 12)) state)
update _ True
  = Nothing

draw :: Splash -> IO()
draw state
  = do
    wipe (gheight + 3) -- 2 + 1 (border, input line)
    render (emborder gBorder (drawOver gCanvas state))
    return ()

anyKey :: [Char] -> Bool
anyKey [] = False
anyKey _ = True
