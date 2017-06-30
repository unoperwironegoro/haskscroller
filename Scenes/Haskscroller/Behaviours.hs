module Scenes.Haskscroller.Behaviours where

import HSCIIEngine.Types

import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.Areas
import Scenes.Haskscroller.Types
import Scenes.Haskscroller.Property

import DataStructures.AdexMap as Adex
import GameCommon

wasdBehaviour :: Behaviour
wasdBehaviour (eid, entity) world actions
  = if moveValid
    then (Adex.update eid movedPlayer world, [])
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
moveBehaviour (eid, entity) world _
  = if moveValid
    then (Adex.update eid movedEntity world, [])
    else (Adex.update eid rmovedEntity world, [])
  where
    disp = read (rProperty entity prV (V2 0 0))
    movedEntity = (moveE fps disp entity)
    disp' = flipY disp
    entity' = wProperty entity prV disp'
    rmovedEntity = moveE fps disp' entity'
    moveValid = heightbox `contains` (hitboxE movedEntity)

-- Restores the id!
despawnBehaviour :: Behaviour
despawnBehaviour (eid, entity) world _
  | offscreen = (world, [eid])
  | otherwise = (world, [])
  where
    offscreen = despawnbox `contains` (hitboxE entity)

spawnerBehaviour :: [SpawnInstruction] -> Behaviour
spawnerBehaviour sis
  = undefined
