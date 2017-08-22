module Game.Src.Haskscroller.Init where

import HSCIIEngine.Types

import Game.Src.Haskscroller.Prefabs
import Game.Src.Haskscroller.Entity
import Game.Src.Haskscroller.World
import Game.Src.Haskscroller.Types
import Game.Src.Haskscroller.Areas

import Game.Src.Common

initState
  = foldl (addEntity) emptyW entities
  where
    entities = [entPlayer, commentSpawner, enemySpawner] ++ commentShower

commentShower
  = zipWith (move 1) positions comments :: [Entity]
  where
    comments = repeat comment
    comment = setVel (setPos (V2 vwidth (-1)) entComment) (V2 (-30) 0)
    positions = take h (scanl1 (+) (repeat ((V2 0.2 1) :: V2F)))
    h = round vheight

commentSpawner
  = setVel (setPos (V2 vwidth (vheight / 2)) entCommentSpawner) (V2 0 (-31.3))

enemySpawner
  = setVel (setPos (V2 vwidth (vheight / 2)) entEnemySpawner) (V2 0 (43.8))
