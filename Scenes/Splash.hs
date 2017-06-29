module Scenes.Splash where

import HSCIIEngine.Engine
import HSCIIEngine.Objects
import HSCIIEngine.Display
import HSCIIEngine.Types

import GameCommon

type Splash = [Object]

splash = tgloop 70 mspf draw anyKey update initState end

initState = [objLogo]

update :: Splash -> Bool -> Splash
update state False
  = map ((flip (move fps)) (0, 12)) state
update _ True
  = skipSplash

draw :: Splash -> IO()
draw state
  = do
    wipe (gheight + 3) -- 2 + 1 (border, input line)
    render (drawOver gCanvas state)
    return ()

anyKey :: [Char] -> Bool
anyKey [] = False
anyKey _ = True

end :: Splash -> Bool
end = (==) skipSplash

skipSplash :: Splash
skipSplash = [((0,0), (0,0), ["Skip Splash Screen"])]
