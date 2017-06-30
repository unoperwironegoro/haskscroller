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
pHP hp = (prHP, show hp)
pV  v  = (prV, show v)

propNone = M.empty


entBind = toEntity imgBind playerSpawn (0,0,3,1)
          [wasdBehaviour]
          propNone

entCommentSpawner = toEntity imgNull nullSpawn (0,0,0,0)
                    [spawnerBehaviour []]
                    propNone

entComment = toEntity imgComment nullSpawn (0,0,2,1)
             [despawnBehaviour, moveBehaviour]
             (toProps [pHP 12, pV (V2 (-1) 0)])
