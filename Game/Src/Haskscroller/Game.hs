module Game.Src.Haskscroller.Game where

import HSCIIEngine.Engine
import HSCIIEngine.Objects
import HSCIIEngine.Display

import Game.Src.Haskscroller.Types
import Game.Src.Haskscroller.World
import Game.Src.Haskscroller.Areas
import Game.Src.Haskscroller.Init
import Game.Src.Haskscroller.HUD
import qualified Game.Src.Haskscroller.Entity as Entity

import Data.Maybe
import qualified Data.Map as M
import qualified DataStructures.AdexMap as Adex

import Game.Src.Common

keyMapping = M.fromList
  [('\ESC', QUIT),

                ('w', UP),
   ('a', LEFT), ('s', DOWN), ('d', RIGHT),          (' ', SELECT)]

game = gloop mspf draw keyhdl update (Just initState)

update :: World -> [Action] -> Maybe World
update world actions
  = if QUIT `elem` actions
      then Nothing
      else Just $ reap (update' world (Adex.toList world))
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
    render display
    return ()
  where
    display = drawOver borderedCanvas hudObjs
    borderedCanvas = emborder gBorder worldCanvas
    worldCanvas = drawOver gCanvas entityObjs
    hudObjs = [hpBar, separatorBar]
    entityObjs = [ obj | (Ent _ _ _ obj) <- (getEntities world)]

    hpBar = createHPBar hpBarPos 10 12

-- TODO handle escape codes
keyhdl :: [Char] -> [Action]
keyhdl
  = catMaybes . (map ((flip M.lookup) keyMapping))
