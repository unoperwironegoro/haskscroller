module Scenes.Haskscroller.Areas where

import HSCIIEngine.Types
import Scenes.Haskscroller.Types

import GameCommon
--           _____________________________________________
--           |............................................|
--           |...._______________________..________.......|
-- Despawn   |....|  Visible area|       |.|~Spawn~|......|
-- area      |....| <------------|-----> |.|~area~~|......|
--           |....|              |       |.|<----->|......|
--           |....| Player area  |       |.|~~~~~~~|......|
--           |....| <--------->  |       |.|~~~~~~~|......|
--           |............................................|
--           |. Entity area...............................|
--            <------------------------------------------>

visArea@(vtl, vbr@(V2 vwidth vheight))
  = (v2fzero, fmap (fromIntegral) gdim) :: Hitbox

--  Entity spawn area
spawnArea@(_, sbr)
  = relativeHitbox (V2 (vwidth + 1) 0) 10 vheight

-- Entity bounds area
entityArea
  = (vtl - leeway, sbr + leeway)
  where
    leeway = V2 6 6

-- Player permitted movement area
playerArea
  = relativeHitbox v2fzero (vwidth - 32) vheight

-- Vertical permitted movement area
bigF = 999999 :: Float

vmoveArea
  = ((V2 (-bigF) 0), (V2 bigF vheight))

-- 'Infinite' area
openArea
  = ((V2 (-bigF) (-bigF)), (V2 bigF bigF))

-- Preset positions
playerSpawn = (V2 5 15) :: V2F
nullSpawn = (V2 (-1) (-1)) :: V2F
