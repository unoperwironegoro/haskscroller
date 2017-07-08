module Scenes.Haskscroller.Behaviours where

import HSCIIEngine.Types

import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.Areas
import Scenes.Haskscroller.Types
import Scenes.Haskscroller.Property
import Scenes.Haskscroller.World

import GameCommon

wasdBehaviour :: Behaviour
wasdBehaviour (eid, entity) _ world actions
  = if moveValid
    then (updateW world eid movedPlayer, [])
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
moveBehaviour (eid, entity) _ world _
  = (updateW world eid movedEntity, [])
  where
    movedEntity = if moveValid
                  then testMovedEntity
                  else revMovedEntity
    testMovedEntity = moveE fps vel entity
    vel = getVel entity (V2 0 0)
    moveValid = heightbox `contains` (hitboxE testMovedEntity)
    -- "Bounce" off the wall
    revVel = flipY vel
    revEntity = setVel entity revVel
    revMovedEntity = moveE fps revVel revEntity

-- Restores the id!
despawnBehaviour :: Behaviour
despawnBehaviour (eid, entity) _ world _
  | offscreen = (world, [eid])
  | otherwise = (world, [])
  where
    offscreen = despawnboxL `contains` (hitboxE entity) ||
                despawnboxR `contains` (hitboxE entity)

spawnerBehaviour :: [SpawnInstruction] -> Behaviour -> Behaviour
spawnerBehaviour [] postBehaviour = postBehaviour
spawnerBehaviour ((e, d):sis) postBehaviour
  | d > 0    = wait
  | otherwise = spawnerBehaviour'
  where
    wait :: Behaviour
    wait (eid, spawner) bid world _
      = (updateW world eid spawner', [])
      where
        spawner' = updateBehaviour spawner bid spawnerBehaviour''
        spawnerBehaviour'' = spawnerBehaviour ((e, (d-1)):sis) postBehaviour

    spawnerBehaviour' :: Behaviour
    spawnerBehaviour' (eid, spawner) bid world _
      = (updateW world' eid spawner', [])
      where
        world' = world `addEntity` newEntity
        newEntity = setRelPosE spawner e
        spawner' = updateBehaviour spawner bid spawnerBehaviour''
        spawnerBehaviour'' = spawnerBehaviour sis postBehaviour

nullBehaviour :: Behaviour
nullBehaviour (eid, entity) bid world _
  = (updateW world eid (removeBehaviour entity bid), [])
------------------- Helpers
