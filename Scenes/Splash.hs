module Scenes.Splash where

import HSCIIEngine.Engine
import HSCIIEngine.Objects
import HSCIIEngine.Display
import HSCIIEngine.Types
import GameCommon

type Splash = [Object]

splashloop = gloop mspf 70 renderSplash anyKey splash initState splashFin

initState = [objLogo]

splash :: Splash -> Bool -> Splash
splash state False
  = map ((flip (move fps)) (0, 12)) state
splash _ True
  = skipSplash

renderSplash :: Splash -> IO()
renderSplash state
  = do
    wipe (gheight + 3) -- 2 + 1 (border, input line)
    render (drawOver gCanvas state)
    return ()

anyKey :: [Char] -> Bool
anyKey [] = False
anyKey _ = True

skipSplash :: Splash
skipSplash = [((0,0), (0,0), ["Skip Splash Screen"])]

splashFin :: Splash -> Bool
splashFin = (==) skipSplash
