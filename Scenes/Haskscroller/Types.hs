module Scenes.Haskscroller.Types where

import HSCIIEngine.Types

import Data.Map

type World = (Map ID Entity, [ID])
data Action = UP | DOWN | LEFT | RIGHT | SELECT deriving (Eq)

type Behaviour = ((ID, Entity) -> World -> [Action] -> World)
type Hitbox = (V2F, V2F) -- Corners: top-left, bottom-right
type PropStr = (String, String)
type PropInt = (String, Int)
type Properties = ([PropStr], [PropInt])
type ID = Int
data Entity = Ent Properties [Behaviour] Hitbox Object

contains :: Hitbox -> Hitbox -> Bool
contains outerbox (tlpos, brpos)
  = outerbox `containsPos` tlpos && outerbox `containsPos` brpos

containsPos :: Hitbox -> V2F -> Bool
containsPos (tlpos, brpos) pos
  = xbounded && ybounded
  where
    boundBelow = v2op (>=) pos tlpos
    boundAbove = v2op (<=) pos brpos
    (V2 xbounded ybounded) = v2op (&&) boundBelow boundAbove
