module Game.Src.Haskscroller.Property where

import Game.Src.Haskscroller.Types

import Data.Maybe
import qualified Data.Map as M

findProperty = M.lookup
updateProperty = M.insert

tagHP = "health"
tagV  = "velocity"
tagO  = "owner"

rProperty :: (Show a) => Entity -> String -> a -> String
rProperty (Ent props _ _ _) key vdefault
  = fromMaybe (show vdefault) (findProperty key props)

wProperty :: (Show a) => Entity -> String -> a -> Entity
wProperty (Ent props bs hb obj) key val
  = (Ent props' bs hb obj)
  where
    props' = updateProperty key (show val) props
