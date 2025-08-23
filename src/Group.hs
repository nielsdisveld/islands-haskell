module Group (Group (..), toGroups, mergeGroups, getComponents) where

import Data.Set (Set)
import Data.Set qualified as Set
import Line (Line)

data Group = Group [Int] (Set Int)
  deriving (Show)

class Connected a where
  (<->) :: a -> a -> Bool

instance (Ord a) => Connected (Set a) where
  (<->) s1 = (/=) Set.empty . Set.intersection s1

instance Connected Group where
  (<->) (Group _ xs) (Group _ ys) = xs <-> ys

mergeGroups :: Group -> Group -> Group
mergeGroups (Group js s1) (Group ks s2) =
  Group (js <> ks) (Set.union s1 s2)

getComponents :: [Group] -> [Group]
getComponents [] = []
getComponents xs = go [] xs
  where
    go acc [] = acc
    go acc (g : gs) =
      case findConnected g gs of
        (Just conn, rest) -> go acc (mergeGroups conn g : rest)
        (Nothing, _) -> go (g : acc) gs

findConnected :: Group -> [Group] -> (Maybe Group, [Group])
findConnected _ [] = (Nothing, [])
findConnected x (y : ys)
  | x <-> y = (Just y, ys)
  | otherwise = (mConn, y : zs)
  where
    (mConn, zs) = findConnected x ys

toGroups :: Line -> [Group]
toGroups = go 1 []
  where
    go _ acc [] = acc
    go i acc (0 : xs) = go i acc xs
    go i acc xs = go (i + 1) (toGroup i group : acc) rest
      where
        (group, rest) = break (0 ==) xs

toGroup :: Int -> Line -> Group
toGroup label s1 = Group [label] (Set.fromList s1)
