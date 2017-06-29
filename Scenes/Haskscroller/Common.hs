module Scenes.Haskscroller.Common where

import HSCIIEngine.Types

import GameCommon

wbox@(wtlpos, wbrpos)
  = (v2fzero, fmap (fromIntegral . (+(-1))) gdim :: V2F)

pbox@(ptlpos, pbrpos)
  = (v2fzero, wbrpos - (V2 8 0))

playerSpawn = (V2 5 15) :: V2F
