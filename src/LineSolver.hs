module LineSolver (solveLine) where

import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Group (Group (..), getComponents, toGroups)
import Line (Line)

solveLine :: Line -> Line
solveLine ln =
  let groups = toGroups ln
      components = getComponents groups
   in toLine ln components

toLine :: Line -> [Group] -> Line
toLine ln groups = go 1 ln
  where
    go _ [] = []
    go i (0 : xs) = 0 : go i xs
    go i (x : xs) = lu i : markNew i xs
    markNew i [] = []
    markNew i (0 : xs) = 0 : go (i + 1) xs
    markNew i (x : xs) = lu i : markNew i xs
    lu k = fromJust (Map.lookup k newLabelLookup)
    newLabelLookup = Map.fromList (concatMap (\(i, Group js _) -> fmap (,i) js) zipped)
    zipped = zip [1 ..] groups
