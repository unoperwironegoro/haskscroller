module Scenes.Haskscroller.Areas where

import HSCIIEngine.Types
import Scenes.Haskscroller.Types

import GameCommon

wbox@(wtlpos, wbrpos@(V2 wwidth wheight))
  = (v2fzero, fmap (fromIntegral) gdim) :: Hitbox

--  Entity spawn area
spawnbox
  = relativeHitbox (V2 (wwidth + 1) 0) 10 wheight

-- Entity despawn area
despawnbox
  = ((V2 (-bigF) (-bigF)), (V2 0 bigF)) :: Hitbox

-- Player permitted movement area
pbox
  = relativeHitbox v2fzero (wwidth - 32) wheight

heightbox
  = relativeHitbox (V2 (-bigF) 0) (bigF * 2) wheight

playerSpawn = (V2 5 15) :: V2F
nullSpawn = (V2 (-1) (-1)) :: V2F

bigF = 999999 :: Float
