module Scenes.Haskscroller.Game where

import HSCIIEngine.Engine
import HSCIIEngine.Objects
import HSCIIEngine.Display

import Scenes.Haskscroller.Types
import Scenes.Haskscroller.World
import Scenes.Haskscroller.Areas
import Scenes.Haskscroller.Init
import qualified Scenes.Haskscroller.Entity as Entity

import Data.Maybe
import qualified Data.Map as M
import qualified DataStructures.AdexMap as Adex

import GameCommon

keyMapping = M.fromList
  [('\ESC', QUIT),

                ('w', UP),
   ('a', LEFT), ('s', DOWN), ('d', RIGHT),          (' ', SELECT)]

game = gloop mspf draw keyhdl update (Just initState)

update :: World -> [Action] -> Maybe World
update world actions
  = if QUIT `elem` actions
      then Nothing
      else Just (reap (update' world (Adex.toList world)))
  where
    -- TODO consider set of dead entities
    update' :: World -> [(ID, Entity)] -> (World, [ID])
    update' w [] = (w, [])
    update' w (ie:ies)
      = (w'', deads ++ recdeads)
      where
        (w'', recdeads) = update' w' ies
        (w', deads) = Entity.update w actions ie

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
