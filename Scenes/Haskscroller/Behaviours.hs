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
    then ((insert eid movedPlayer ies, freeids), [])
    else (world, [])
  where
    movedPlayer = (moveE 1 disp entity)
    disp = V2 dx dy
    dy = axis DOWN UP
    dx = axis RIGHT LEFT
    axis forward backward
      = if backward `elem` actions then -1 else
        if forward `elem` actions then 1 else 0
    moveValid
      = pbox `contains` (hitboxE movedPlayer)

-- Restores the id!
despawnBehaviour :: Behaviour
despawnBehaviour (eid, entity) world@(ies, freeids) _
  | offscreen = (world, [eid])
  | otherwise = (world, [])
  where
    offscreen = despawnbox `contains` (hitboxE entity)
