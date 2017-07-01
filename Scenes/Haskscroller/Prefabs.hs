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
    [spawnerBehaviour []]
    propNone

entComment
  = createEntity imgComment
    nullSpawn (0,0,2,1)
    [despawnBehaviour, moveBehaviour]
    (toProps [propHP 12, propV (V2 (-1) 0)])
