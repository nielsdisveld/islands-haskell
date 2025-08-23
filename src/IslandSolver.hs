module IslandSolver (islands) where

import Line (Line, distinct, zeroes, (*.), (+.))
import LineSolver (solveLine)

islands :: [Line] -> Int
islands (x : xs) = fst (go zeroes' (foldr go (0, zeroes') (x : xs)))
  where
    zeroes' = x *. zeroes
    go ln (acc, ln0) =
      let closed = closedIslands ln0 ln
          upped = ([maximum ln0 + 1 ..] +. ln) *. ln
          solvedLine = solveLine (mergeLines ln0 upped)
       in (acc + closed, solvedLine)

closedIslands :: Line -> Line -> Int
closedIslands ln1 ln2 = distinct ln1 - distinct (ln1 *. ln2)

mergeLines :: Line -> Line -> Line
mergeLines [] _ = []
mergeLines _ [] = []
mergeLines (x : xs) (y : ys) = z : mergeLines xs ys
  where
    z = case (x, y) of
      (x, 0) -> 0
      (0, y) -> y
      (x, _) -> x
