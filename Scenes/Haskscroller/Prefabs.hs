module Scenes.Haskscroller.Prefabs where

import Resources.Sprites
import Scenes.Haskscroller.Entity
import Scenes.Haskscroller.Behaviours
import Scenes.Haskscroller.Areas
import Data.Map as M

propMobile hp mdir =
  M.fromList [("Health", show hp),
              ("MovDir", mdir)]

entBind = toEntity imgBind playerSpawn (0, 0, 3, 1) [wasdBehaviour] M.empty
