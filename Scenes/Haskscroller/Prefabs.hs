module Scenes.Haskscroller.Prefabs where

import HSCIIEngine.Types

import Resources.Sprites
import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.Behaviours
import Scenes.Haskscroller.Areas
import Scenes.Haskscroller.Property
import Scenes.Haskscroller.Types

import Data.Map as M

toProps = M.fromList :: [(String, String)] -> Properties
propHP hp  = (tagHP, show hp)
propV  vel = (tagV , show vel)

propNone = M.empty

entPlayer
  = createEntity imgBind
    playerSpawn (0,0,3,1)
    [wasdBehaviour]
    propNone

entCommentSpawner
  = createEntity imgNull
    nullSpawn (0,0,0,0)
    [spawnInfComments, moveBehaviour]
    propNone

entComment
  = createEntity imgComment
    nullSpawn (0,0,2,1)
    [despawnBehaviour, moveBehaviour]
    (toProps [propHP 12, propV (V2 (-1) 0)])

------------------- Spawner Instructions

spawnInfComments
  = spawnerBehaviour (cycle pattern) nullBehaviour
  where
    pattern = zipWith (\c d -> (c, d)) comments delays
    comments = zipWith (\c vx -> setVel c (V2 vx 0)) (repeat entComment) vxs
    vxs = cycle [-5, -6, -3, -7, -2, -4]
    delays = cycle [30, 36, 25, 40, 37]
