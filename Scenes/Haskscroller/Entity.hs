module Scenes.Haskscroller.Entity where

import HSCIIEngine.Types
import HSCIIEngine.Objects

import Scenes.Haskscroller.Types
import Data.Maybe
import Data.Map

toEntity :: Image -> Coords -> Hitbox -> [Behaviour] -> Properties
         -> Entity
toEntity img coords hitbox behaviours props
  = Ent props behaviours hitbox (toObject img coords)

addEntity :: World -> Entity -> World
addEntity (entities, (nextID:ids)) newEntity
  = (insert nextID newEntity entities, ids)

moveE :: Float -> Coords -> Entity -> Entity
moveE scale coords (Ent ps bs h obj)
  = Ent ps bs h (move scale obj coords)

updateE :: World -> [Action] -> (ID, Entity) -> World
updateE world actions (eID, Ent _ behaviours _ _)
  = update' behaviours world
  where
    update' :: [Behaviour] -> World -> World
    update' [] w = w
    update' (b:bs) w@(ies, _)
      = update' bs w'
      where
        w' = b ie w actions
        ie = (eID, fromJust (Data.Map.lookup eID ies))
