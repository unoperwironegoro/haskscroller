module Scenes.Haskscroller.Behaviours where

import HSCIIEngine.Types

import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.Areas
import Scenes.Haskscroller.Types

import Data.Map

wasdBehaviour :: Behaviour
wasdBehaviour (eid, entity)
              world@(ies, freeids) actions
  = if moveValid
    then ((adjust (movePlayer disp) eid ies, freeids), [])
    else (world, [])
  where
    movePlayer = moveE 1
    disp = V2 dx dy
    dy = axis DOWN UP
    dx = axis RIGHT LEFT
    axis forward backward
      = if backward `elem` actions then -1 else
        if forward `elem` actions then 1 else 0
    moveValid
      = pbox `containsPos` ((posE entity) + disp)

-- Restores the id!
despawnBehaviour :: Behaviour
despawnBehaviour (eid, entity) world@(ies, freeids) _
  | offscreen = (world, [eid])
  | otherwise = (world, [])
  where
    offscreen = despawnbox `contains` (hitboxE entity)
