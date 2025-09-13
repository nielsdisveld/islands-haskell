module Solver (solve) where

import Line (Line, zeroes)

-- | Helper type that represents the previous line and the current line
-- that is to be evaluated. The first coordinate of a point represents entry on
-- previous line.
type TwoLines = [(Int, Bool)]

-- | Helper type that represents the state when traversing two (zipped) lines looking for islands
-- None means we are currently seeing zeroes on both lines
-- New means we have only seen land on the new line
-- ConnectedPrev x, ConnectedCurr x, ConnectedBoth x means the land we are exploring is connected
-- to land with label x on the previous line.
data IslandState = None | New | ConnectedPrev Int | ConnectedCurr Int | ConnectedBoth Int
  deriving (Eq)

-- | Count how many islands on given input.
solve :: [Line Bool] -> Int
solve = fst . foldr go (0, zeroes)
  where
    go ln (acc, prev) =
      let twoLines = zip prev ln
          (new, transformed) = countNew twoLines
       in (acc + new, transformed)

-- | Compare line to previous line and count how many new islands are starting. Also return a
-- transformed version of current line where the islands are labeled
countNew :: TwoLines -> (Int, Line Int)
countNew = countNew' (0, 0) None

countNew' :: (Int, Int) -> IslandState -> [(Int, Bool)] -> (Int, Line Int)
countNew' (count, _) New [] = (count + 1, [])
countNew' (count, _) _ [] = (count, [])
countNew' (count, slot) prevState (p : ps) =
  let (nextState, incrSlot, dcount) = next prevState p
      nextSlot = if incrSlot then slot + 1 else slot
      transformedp =
        case nextState of
          None -> 0
          ConnectedPrev _ -> 0
          _ -> nextSlot
      (allCount, transformedLine) = countNew' (count + dcount, nextSlot) nextState ps
   in (allCount, transformedp : transformedLine)

-- | Calculate all state transitions and return a tuple of the
-- 1. New state
-- 2. A boolean that represents if the label should be changed when dealing with the next island
-- 3. An integer that represents the total count difference (0,1 or -1)
next :: IslandState -> (Int, Bool) -> (IslandState, Bool, Int)
next New (0, False) = (None, False, 1) -- Impact on count since we found a new island
next _ (0, False) = (None, False, 0)
next None (0, True) = (New, True, 0)
next None (x, True) = (ConnectedBoth x, True, 0)
next None (x, False) = (ConnectedPrev x, True, 0)
next New (0, True) = (New, False, 0)
next New (x, True) = (ConnectedBoth x, False, 0)
next New (x, False) = (ConnectedPrev x, True, 1) -- Impact on count since we found a new island
next (ConnectedPrev y) (0, True) = (New, True, 0)
next (ConnectedPrev y) (x, True) = (ConnectedBoth y, False, 0)
next (ConnectedPrev y) (x, False) = (ConnectedPrev y, False, 0)
next (ConnectedCurr y) (0, True) = (ConnectedCurr y, False, 0)
next (ConnectedCurr y) (x, True)
  | x == y = (ConnectedBoth y, False, 0)
  -- Impact on count since two different islands on previous line were connected by the new line
  -- so total number of islands should decrease by 1
  | x /= y = (ConnectedBoth y, False, -1)
next (ConnectedCurr y) (x, False) = (ConnectedPrev x, True, 0)
next (ConnectedBoth y) (0, True) = (ConnectedCurr y, False, 0)
next (ConnectedBoth y) (x, True) = (ConnectedBoth y, False, 0)
next (ConnectedBoth y) (x, False) = (ConnectedPrev y, False, 0)
