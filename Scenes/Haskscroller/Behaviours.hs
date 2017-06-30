module Scenes.Haskscroller.Behaviours where

import HSCIIEngine.Types

import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.Areas
import Scenes.Haskscroller.Types
import Scenes.Haskscroller.Property

import DataStructures.AdexMap
import GameCommon

import qualified Data.Map as M

wasdBehaviour :: Behaviour
wasdBehaviour (eid, entity) world@(ies, freeids) actions
  = if moveValid
    then ((M.insert eid movedPlayer ies, freeids), [])
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

-- TODO properly handle "not enclosed" case
moveBehaviour :: Behaviour
moveBehaviour (eid, entity) world@(ies, freeids) _
  = if moveValid
    then ((M.insert eid movedEntity ies, freeids), [])
    else ((M.insert eid rmovedEntity ies, freeids), [])
  where
    disp = read (rProperty entity prV (V2 0 0))
    movedEntity = (moveE fps disp entity)
    disp' = flipY disp
    entity' = wProperty entity prV disp'
    rmovedEntity = moveE fps disp' entity'
    moveValid = heightbox `contains` (hitboxE movedEntity)

-- Restores the id!
despawnBehaviour :: Behaviour
despawnBehaviour (eid, entity) world@(ies, freeids) _
  | offscreen = (world, [eid])
  | otherwise = (world, [])
  where
    offscreen = despawnbox `contains` (hitboxE entity)

spawnerBehaviour :: [SpawnInstruction] -> Behaviour
spawnerBehaviour sis
  = undefined
