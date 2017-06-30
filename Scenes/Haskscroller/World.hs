module Scenes.Haskscroller.World where

import Scenes.Haskscroller.Types

import DataStructures.AdexMap as Adex

addEntity :: World -> Entity -> World
addEntity world newEntity
  = add newEntity world

getEntities :: World -> [Entity]
getEntities = Adex.elems

removeEntity :: World -> ID -> World
removeEntity = (flip Adex.delete)
