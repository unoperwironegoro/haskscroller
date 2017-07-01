module Scenes.Haskscroller.Entity where

import HSCIIEngine.Types
import HSCIIEngine.Objects

import Scenes.Haskscroller.Types
import Scenes.Haskscroller.Property

import Data.Maybe
import DataStructures.AdexMap as Adex

createEntity :: Image                        -> -- Sprite
                V2F                          -> -- Initial position
                (Float, Float, Float, Float) -> -- Hitbox x, y, width, height
                [Behaviour]                  -> -- Per-frame update functions
                Properties                   -> -- Tagged Data
                Entity
createEntity img coords (l, t, w, h) bhvs props
  = Ent props bhvs hbx obj
  where
    hbx = relativeHitbox (V2 l t) w h
    obj = (toObject img coords)

moveE :: Float -> V2F -> Entity -> Entity
moveE scale coords (Ent props bhvs hbx obj)
  = Ent props bhvs hbx (move scale obj coords)

setPosE :: V2F -> Entity -> Entity
setPosE coords (Ent props bhvs hbx obj@(pos, _, _))
  = Ent props bhvs hbx (move 1 obj (coords - pos))

updateE :: World -> [Action] -> (ID, Entity) -> (World, [ID])
updateE world actions (eID, entity)
  = update' (behaviours entity) world
  where
    update' :: [Behaviour] -> World -> (World, [ID])
    update' [] w = (w, [])
    update' (b:bs) w
      = (recw, deads ++ recdeads)
      where
        (recw, recdeads) = update' bs w'
        (w', deads) = b ie w actions
        ie = (eID, fromJust (Adex.lookup eID w)) -- TODO remove?

behaviours :: Entity -> [Behaviour]
behaviours (Ent _ bhvs _ _) = bhvs

posE :: Entity -> V2F
posE (Ent _ _ _ (pos, _, _)) = pos

relhitboxE :: Entity -> Hitbox
relhitboxE (Ent _ _ hbx _) = hbx

hitboxE :: Entity -> Hitbox
hitboxE entity
  = moveHB (relhitboxE entity) (posE entity)

moveHB :: Hitbox -> V2F -> Hitbox
moveHB (tlbox, brbox) disp
  = (disp + tlbox, disp + brbox)

------------------- Property Manipulation

getVel :: Entity -> V2F -> V2F
getVel entity defVel
  = read (rProperty entity tagV defVel)

setVel :: Entity -> V2F -> Entity
setVel entity vel
  = wProperty entity tagV vel
