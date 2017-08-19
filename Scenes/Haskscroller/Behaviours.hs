module Scenes.Haskscroller.Behaviours where

import HSCIIEngine.Types

import Scenes.Haskscroller.Areas
import Scenes.Haskscroller.Types
import Scenes.Haskscroller.Property
import Scenes.Haskscroller.World
import Scenes.Haskscroller.Entity

import GameCommon

playeray = 2.0
playerax = 2.5
playervmax = (V2 (playeray * 4) (playeray * 3))

wasdBehaviour :: Behaviour
wasdBehaviour (eid, entity) _ world actions
  = if dv == (V2 0 0)
    then (world, [])
    else (updateW world eid entity', [])
  where
    dv = V2 dx dy
    dx = axis RIGHT LEFT playerax
    dy = axis DOWN  UP   playeray
    axis forward backward scale
      = if backward `elem` actions
          then -scale
          else if forward `elem` actions
            then scale
            else 0
    v = getVel entity (V2 0 0)
    v' = clamp (v + dv) (-playervmax) playervmax
    entity' = setVel entity v'

playerMoveBehaviour = boundedMoveBehaviour playerArea
basicMoveBehaviour = boundedMoveBehaviour vmoveArea

-- TODO properly handle bouncing
boundedMoveBehaviour :: Hitbox -> Behaviour
boundedMoveBehaviour bounds
  = moveBehaviour
  where
    moveBehaviour (eid, entity) _ world _
      = (updateW world eid movedEntity, [])
      where
        movedEntity = if moveValid
                        then testMovedEntity
                        else revMovedEntity
        testMovedEntity = move fps vel entity
        vel = getVel entity (V2 0 0)
        moveValid = vmoveArea `contains` (getHitbox testMovedEntity)
        -- "Bounce" off the wall
        revVel = flipY vel
        revEntity = setVel entity revVel
        revMovedEntity = move fps revVel revEntity

-- Restores the id!
despawnBehaviour :: Behaviour
despawnBehaviour (eid, entity) _ world _
  | offscreen = (world, [eid])
  | otherwise = (world, [])
  where
    offscreen = not (entityArea `contains` (getHitbox entity))

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
        newEntity = setRelPos spawner e
        spawner' = updateBehaviour spawner bid spawnerBehaviour''
        spawnerBehaviour'' = spawnerBehaviour sis postBehaviour

nullBehaviour :: Behaviour
nullBehaviour (eid, entity) bid world _
  = (updateW world eid (removeBehaviour entity bid), [])
------------------- Helpers
