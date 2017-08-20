module Scenes.Haskscroller.Types where

import HSCIIEngine.Types

import qualified DataStructures.AdexMap as Adex
import Data.Map

type World = Adex.AdexMap Entity
data Action = QUIT | UP | DOWN | LEFT | RIGHT | SELECT deriving (Eq)

type Behaviour = ((ID, Entity) -> -- 'this' Entity reference
                  ID           -> -- 'this' Behaviour reference
                  World        -> -- Current world
                  [Action]     -> -- Inputs
                  (World,         -- New world
                    [ID]))        -- Entities to cull
type Behaviours = Adex.AdexMap Behaviour

type Hitbox = (V2F, V2F) -- Corners: top-left, bottom-right
type Properties = Map String String -- TODO replace unhaskellic hack
type ID = Int
data Entity = Ent Properties Behaviours Hitbox Object

type Delay = Int
type SpawnInstruction = (Entity, Delay)

relativeHitbox :: V2F -> Float -> Float -> Hitbox
relativeHitbox pos width height
  = (pos, pos + (V2 width height))

contains :: Hitbox -> Hitbox -> Bool
contains outerbox (tlpos, brpos)
  = (outerbox `containsPos` tlpos) &&
    (outerbox `containsPos` brpos)

containsPos :: Hitbox -> V2F -> Bool
containsPos (tlpos, brpos) pos
  = xbounded && ybounded
  where
    boundBelow = v2op (>=) pos tlpos
    boundAbove = v2op (<=) pos brpos
    (V2 xbounded ybounded) = v2op (&&) boundBelow boundAbove

boxBelow :: Hitbox -> Hitbox -> Bool
boxBelow (_, br1) (_, br2) = br1 `below` br2

boxAbove :: Hitbox -> Hitbox -> Bool
boxAbove (tl1, _) (tl2, _) = tl1 `above` tl2

boxLeftOf :: Hitbox -> Hitbox -> Bool
boxLeftOf (tl1, _) (tl2, _) = tl1 `leftOf` tl2

boxRightOf :: Hitbox -> Hitbox -> Bool
boxRightOf (_, br1) (_, br2) = br1 `rightOf` br2
