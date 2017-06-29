module Scenes.Haskscroller.Game where

import HSCIIEngine.Engine
import HSCIIEngine.Objects
import HSCIIEngine.Display

import Scenes.Haskscroller.Types
import Scenes.Haskscroller.Prefabs
import Scenes.Haskscroller.Entity

import Data.Maybe
import qualified Data.Map as M

import GameCommon

keyMapping = M.fromList
  [             ('w', UP),
  ('a', LEFT), ('s', DOWN), ('d', RIGHT),          (' ', SELECT)]

game = gloop mspf draw keyhdl update initState fin

initState = (M.empty, [1..]) `addEntity` entBind

update :: World -> [Action] -> World
update world@(idEnts, _) actions
  = update' world (M.toList idEnts)
  where
    update' :: World -> [(ID, Entity)] -> World
    update' w [] = w
    update' w (ie:ies)
      = update' (updateE w actions ie) ies

draw :: World -> IO()
draw (idEnts, _)
  = do
    wipe (gheight + 3) -- 2 + 1 (border, input line)
    render (drawOver gCanvas entityObjs)
    return ()
  where
    entityObjs = [ obj | (Ent _ _ _ obj) <- (M.elems idEnts)]

keyhdl :: [Char] -> [Action]
keyhdl
  = catMaybes . (map ((flip M.lookup) keyMapping))

fin :: World -> Bool
fin = (\_ -> False)
