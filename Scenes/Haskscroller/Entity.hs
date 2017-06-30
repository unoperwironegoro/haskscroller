module Scenes.Haskscroller.Entity where

import HSCIIEngine.Types
import HSCIIEngine.Objects

import Scenes.Haskscroller.Types
import Data.Maybe
import Data.Map

toEntity :: Image -> V2F -> (Float, Float, Float, Float) -> [Behaviour] -> Properties
         -> Entity
toEntity img coords (l, t, w, h) behaviours props
  = Ent props behaviours hitbox object
  where
    hitbox = relativeHitbox (V2 l t) w h
    object = (toObject img coords)

addEntity :: World -> Entity -> World
addEntity (entities, (nextID:ids)) newEntity
  = (insert nextID newEntity entities, ids)

moveE :: Float -> V2F -> Entity -> Entity
moveE scale coords (Ent ps bs h obj)
  = Ent ps bs h (move scale obj coords)

setPosE :: V2F -> Entity -> Entity
setPosE coords (Ent ps bs h obj@(pos, _, _))
  = Ent ps bs h (move 1 obj (coords - pos))

updateE :: World -> [Action] -> (ID, Entity) -> (World, [ID])
updateE world actions (eID, Ent _ behaviours _ _)
  = update' behaviours world
  where
    update' :: [Behaviour] -> World -> (World, [ID])
    update' [] w = (w, [])
    update' (b:bs) w@(ies, _)
      = (recw, deads ++ recdeads)
      where
        (recw, recdeads) = update' bs w'
        (w', deads) = b ie w actions
        ie = (eID, fromJust (Data.Map.lookup eID ies))

posE :: Entity -> V2F
posE (Ent _ _ _ (pos, _, _)) = pos

relhitboxE :: Entity -> Hitbox
relhitboxE (Ent _ _ hitbox _) = hitbox

hitboxE :: Entity -> Hitbox
hitboxE entity
  = moveHB (relhitboxE entity) (posE entity)
  
moveHB :: Hitbox -> V2F -> Hitbox
moveHB (tlbox, brbox) disp
  = (disp + tlbox, disp + brbox)
