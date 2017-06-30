module Scenes.Haskscroller.Areas where

import HSCIIEngine.Types
import Scenes.Haskscroller.Types

import GameCommon

wbox@(wtlpos, wbrpos@(V2 wwidth wheight))
  = (v2fzero, fmap (fromIntegral) gdim) :: Hitbox

--  Entity spawn area
spawnbox@(stlpos, sbrpos)
  = relativeHitbox tlpos 10 wheight
  where tlpos = (V2 (wwidth + 1) 0)

-- Entity despawn area
despawnbox@(etlpos, ebrpos)
  = ((V2 (-100) (-100)), (V2 (-1) 100)) :: Hitbox

-- Player permitted movement area
pbox@(ptlpos, pbrpos)
  = relativeHitbox v2fzero (wwidth - 32) wheight

playerSpawn = (V2 5 15) :: V2F
