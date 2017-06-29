module Scenes.Haskscroller.Behaviours where

import HSCIIEngine.Types

import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.Common
import Scenes.Haskscroller.Types

import Data.Map

wasdBehaviour :: Behaviour
wasdBehaviour (eid, entity@(Ent _ _ _ (pos, _, _)))
              world@(ies, freeids) actions
  = if moveValid
    then (adjust (movePlayer disp) eid ies, freeids)
    else world
  where
    movePlayer = moveE 1
    disp = V2 dx dy
    dy = axis DOWN UP
    dx = axis RIGHT LEFT
    axis forward backward
      = if backward `elem` actions then -1 else
        if forward `elem` actions then 1 else 0
    moveValid
      = pbox `containsPos` (pos + disp)
