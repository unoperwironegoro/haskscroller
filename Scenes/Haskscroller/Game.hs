module Scenes.Haskscroller.Game where

import HSCIIEngine.Engine
import HSCIIEngine.Objects
import HSCIIEngine.Display
import HSCIIEngine.Types

import Scenes.Haskscroller.Types
import Scenes.Haskscroller.Prefabs
import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.World

import Data.Maybe
import qualified Data.Map as M
import qualified DataStructures.AdexMap as Adex

import GameCommon

keyMapping = M.fromList
  [             ('w', UP),
  ('a', LEFT), ('s', DOWN), ('d', RIGHT),          (' ', SELECT)]

game = gloop mspf draw keyhdl update initState fin

initState = Adex.empty
            `addEntity` entBind
            `addEntity` (setPosE (V2 20 7) entComment)

update :: World -> [Action] -> World
update world actions
  = reap (update' world (Adex.toList world))
  where
    -- TODO consider set of dead entities
    update' :: World -> [(ID, Entity)] -> (World, [ID])
    update' w [] = (w, [])
    update' w (ie:ies)
      = (w'', deads ++ recdeads)
      where
        (w'', recdeads) = update' w' ies
        (w', deads) = updateE w actions ie

    reap :: (World, [ID]) -> World
    reap (w, deads) = foldl removeEntity w deads

draw :: World -> IO()
draw world
  = do
    wipe (gheight + 3) -- 2 + 1 (border, input line)
    render (drawOver gCanvas entityObjs)
    return ()
  where
    entityObjs = [ obj | (Ent _ _ _ obj) <- (getEntities world)]

keyhdl :: [Char] -> [Action]
keyhdl
  = catMaybes . (map ((flip M.lookup) keyMapping))

fin :: World -> Bool
fin = (\_ -> False)
