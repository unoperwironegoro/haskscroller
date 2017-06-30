module Scenes.Haskscroller.Property where

import Scenes.Haskscroller.Types

import Data.Maybe
import qualified Data.Map as M

findProperty = M.lookup
updateProperty = M.insert

prHP = "health"
prV = "velocity"

rProperty :: (Show a) => Entity -> String -> a -> String
rProperty (Ent props _ _ _) key vdefault
  = fromMaybe (show vdefault) (findProperty key props)

wProperty :: (Show a) => Entity -> String -> a -> Entity
wProperty (Ent props bs hb obj) key val
  = (Ent props' bs hb obj)
  where
    props' = updateProperty key (show val) props
