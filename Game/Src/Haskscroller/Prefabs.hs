module Game.Src.Haskscroller.Prefabs where

import HSCIIEngine.Types

import Game.Src.Haskscroller.Entity
import Game.Src.Haskscroller.Behaviours
import Game.Src.Haskscroller.Areas
import Game.Src.Haskscroller.Property
import Game.Src.Haskscroller.Types

import Game.Src.Common

import qualified Game.Res.Sprites as Spr

import qualified Data.Map as M

toProps = M.fromList :: [(String, String)] -> Properties
propHP hp  = (tagHP, show hp)
propV  vel = (tagV , show vel)
propO  o   = (tagO , show o)

propNone = M.empty

entPlayer
  = createEntity Spr.ePlayer
    playerSpawn (0,0,width,1)
    [wasdBehaviour, playerMoveBehaviour,
     (spawnProjectiles (V2 25 0) (V2 width 0.0)),
     (hpBehaviour 10)]
    propNone
  where
    width = 10
--TODO move?
spawnProjectiles v offset
  = spawnerBehaviour projs nullBehaviour
  where
    projs = repeat (entProjectile v offset, delay)
    delay = secToFrames 1

entProjectile v pos
  = createEntity Spr.eProjectile
    pos (0, 0, 1, 1)
    [despawnBehaviour, basicMoveBehaviour]
    (toProps [propV v])

---------------------------- Hostile Entities ------------------------------
entEnemyConst
  = createEntity Spr.eConst
    nullSpawn (0, 0, 8, 1)
    [despawnBehaviour, basicMoveBehaviour]
    propNone

--------------------------- Decorative Entities ----------------------------
entComment
  = createEntity Spr.eComment
    nullSpawn (0,0,2,1)
    [despawnBehaviour, basicMoveBehaviour]
    (toProps [propV (V2 (-1) 0)])

--------------------------- Offscreen Spawners -----------------------------
entCommentSpawner
  = createEntity Spr.blank
    nullSpawn (0,0,0,0)
    [spawnInfComments, basicMoveBehaviour]
    propNone
  where
    spawnInfComments = spawnerBehaviour (cycle pattern) nullBehaviour
    pattern = zipWith (\c d -> (c, d)) comments delays
    comments = zipWith (\c vx -> setVel c (V2 vx 0)) (repeat entComment) vxs
    vxs = cycle [-20, -24, -12, -28, -8, -16]
    delays = cycle (map secToFrames [1.0, 1.1, 0.8, 1.4, 1.3])

entEnemySpawner
  = createEntity Spr.blank
    nullSpawn (0,0,0,0)
    [spawnEnemies0, basicMoveBehaviour]
    propNone
  where
    spawnEnemies0 = spawnerBehaviour (take 30 pattern) spawnEnemies0
    pattern = zipWith (\c d -> (c, d)) enemies delays
    enemies = (repeat (setVel entEnemyConst v))
    delays = cycle (map secToFrames [3.2, 4.3, 2.7, 2.9, 3.8, 3.6])
    v = V2 (-8) 0
