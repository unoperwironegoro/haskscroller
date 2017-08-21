module Game.Src.Haskscroller.World where

import Game.Src.Haskscroller.Types

import DataStructures.AdexMap as Adex

emptyW = Adex.empty :: World

addEntity :: World -> Entity -> World
addEntity world newEntity
  = add newEntity world

getEntities :: World -> [Entity]
getEntities = Adex.elems

removeEntity :: World -> ID -> World
removeEntity = (flip Adex.delete)

updateW :: World -> ID -> Entity -> World
updateW = \world eid entity -> Adex.update eid entity world
