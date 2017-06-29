module Scenes.Haskscroller.Prefabs where

import Resources.Sprites
import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.Behaviours
import Scenes.Haskscroller.Common

entBind = toEntity imgBind playerSpawn (0, 0, 3, 1) [wasdBehaviour] ([],[])
