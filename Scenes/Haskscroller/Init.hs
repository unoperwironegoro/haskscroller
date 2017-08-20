module Scenes.Haskscroller.Init where

import HSCIIEngine.Types

import Scenes.Haskscroller.Prefabs
import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.World
import Scenes.Haskscroller.Types
import Scenes.Haskscroller.Areas

import GameCommon

initState
  = foldl (addEntity) emptyW entities
  where
    entities = [entPlayer, commentSpawner] ++ commentShower

commentShower
  = zipWith (move 1) positions comments :: [Entity]
  where
    comments = repeat comment
    comment = setVel (setPos (V2 vwidth (-1)) entComment) (V2 (-10) 0)
    positions = take h (scanl1 (+) (repeat ((V2 0.2 1) :: V2F)))
    h = round vheight

commentSpawner
  = setVel (setPos (V2 vwidth (vheight / 2)) entCommentSpawner) (V2 0 (-30))
