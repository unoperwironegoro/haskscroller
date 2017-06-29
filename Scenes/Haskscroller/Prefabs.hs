module Scenes.Haskscroller.Prefabs where

import Resources.Sprites
import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.Types

import Data.Map

entBind = toEntity imgBind (5, 15) ((0, 0), (3, 1)) [wasdBehaviour] ([],[])

wasdBehaviour :: Behaviour
wasdBehaviour (eid, entity) world@(ies, freeids) actions
  | UP    `elem` actions = worldPlayerMoved ( 0,-1)
  | DOWN  `elem` actions = worldPlayerMoved ( 0, 1)
  | LEFT  `elem` actions = worldPlayerMoved (-1, 0)
  | RIGHT `elem` actions = worldPlayerMoved ( 1, 0)
  | otherwise            = world
  where
    movePlayer = moveE 1
    worldPlayerMoved coords
      = (adjust (movePlayer coords) eid ies, freeids)
