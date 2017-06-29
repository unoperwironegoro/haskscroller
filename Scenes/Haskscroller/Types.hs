module Scenes.Haskscroller.Types where

import HSCIIEngine.Types

import Data.Map

type World = (Map ID Entity, [ID])
data Action = UP | DOWN | LEFT | RIGHT | SELECT deriving (Eq)

type Behaviour = ((ID, Entity) -> World -> [Action] -> World)
type Hitbox = (Coords, Coords) -- Corners: top-left, bottom-right
type PropStr = (String, String)
type PropInt = (String, Int)
type Properties = ([PropStr], [PropInt])
type ID = Int
data Entity = Ent Properties [Behaviour] Hitbox Object
