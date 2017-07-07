module DataStructures.AdexMap where

import qualified Data.Map as M
import Data.Maybe

type AdexMap v = (M.Map Int v, [Int])

empty = (M.empty, [1..] :: [Int])

-- Assume not in map
add :: a -> AdexMap a -> AdexMap a
add v (m, (i:ii)) = (M.insert i v m, ii)

-- Assume in map
delete :: Int -> AdexMap a -> AdexMap a
delete i (m, ii) = (M.delete i m, (i:ii))

-- Assume in map
update :: Int -> a -> AdexMap a -> AdexMap a
update i v (m, is) = (M.insert i v m, is)

toList :: AdexMap a -> [(Int, a)]
toList = (M.toList . fst)

fromVals :: [a] -> AdexMap a
fromVals vs = foldl (flip add) empty vs 

lookup :: Int -> AdexMap a -> Maybe a
lookup i (m, _) = M.lookup i m

elems :: AdexMap a -> [a]
elems = (M.elems . fst)
