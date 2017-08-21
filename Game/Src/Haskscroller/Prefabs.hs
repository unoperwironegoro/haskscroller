module Game.Src.Haskscroller.Prefabs where

import HSCIIEngine.Types

import Game.Src.Haskscroller.Entity
import Game.Src.Haskscroller.Behaviours
import Game.Src.Haskscroller.Areas
import Game.Src.Haskscroller.Property
import Game.Src.Haskscroller.Types

import qualified Game.Res.Sprites as Spr

import Data.Map as M

toProps = M.fromList :: [(String, String)] -> Properties
propHP hp  = (tagHP, show hp)
propV  vel = (tagV , show vel)

propNone = M.empty

entPlayer
  = createEntity Spr.eBind
    playerSpawn (0,0,3,1)
    [wasdBehaviour, playerMoveBehaviour, (hpBehaviour 10)]
    propNone

entCommentSpawner
  = createEntity Spr.blank
    nullSpawn (0,0,0,0)
    [spawnInfComments, basicMoveBehaviour]
    propNone

entComment
  = createEntity Spr.eComment
    nullSpawn (0,0,2,1)
    [despawnBehaviour, basicMoveBehaviour]
    (toProps [propHP 12, propV (V2 (-1) 0)])

------------------- Spawner Instructions
spawnInfComments
  = spawnerBehaviour (cycle pattern) nullBehaviour
  where
    pattern = zipWith (\c d -> (c, d)) comments delays
    comments = zipWith (\c vx -> setVel c (V2 vx 0)) (repeat entComment) vxs
    vxs = cycle [-5, -6, -3, -7, -2, -4]
    delays = cycle [30, 36, 25, 40, 37]
