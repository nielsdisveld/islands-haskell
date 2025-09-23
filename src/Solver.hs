module Solver (solve) where

import Line (Line, zeroes)

type ComparePoints = (Int, Bool)

type CompareLines = Line ComparePoints

data IslandState = None | New | Connected Int
  deriving (Eq, Show)

-- | Count how many islands on given input.
solve :: [Line Bool] -> Int
solve = (\(a, _, _) -> a) . foldr go (0, 1, zeroes)
  where
    go ln (count, slot, prev) =
      let twoLines = zip prev ln
       in countNew count slot twoLines

-- | Compare line to previous line and count how many new islands are starting. Also return a
-- transformed version of current line where the islands are labeled
countNew :: Int -> Int -> CompareLines -> (Int, Int, Line Int)
countNew count slot lns =
  case splitConnected lns of
    ([], []) -> (count, slot, [])
    (land, rest) ->
      let initial = initialState (head land)
          (newCount, state) = countNew' count initial land
          (transformed, newSlot) = transform slot state land
          (finalCount, finalSlot, finalTransformed) = countNew newCount newSlot rest
       in (finalCount, finalSlot, transformed ++ finalTransformed)

countNew' :: Int -> IslandState -> CompareLines -> (Int, IslandState)
countNew' count state (p1 : p2 : rest) =
  case (state, next state p1 p2) of
    (Connected x, Connected y) | x /= y -> countNew' (count - 1) state (p2 : rest)
    (_, nextState) -> countNew' count nextState (p2 : rest)
countNew' count New _ = (count + 1, New)
countNew' count state _ = (count, state)

splitConnected :: CompareLines -> (CompareLines, CompareLines)
splitConnected [] = ([], [])
splitConnected [x] = ([x], [])
splitConnected (x : y : xs) =
  if isConnected x y
    then
      let (land, rest) = splitConnected (y : xs)
       in (x : land, rest)
    else ([x], y : xs)

isConnected :: ComparePoints -> ComparePoints -> Bool
isConnected p1 p2 =
  case (p1, p2) of
    ((0, False), _) -> True
    (_, (0, False)) -> False
    _ | p1 == p2 -> True
    ((_, True), (_, True)) -> True
    ((0, True), (_, False)) -> False
    ((_, False), (0, True)) -> False
    ((x, _), (y, _)) | x == y -> True

initialState :: ComparePoints -> IslandState
initialState (0, False) = None
initialState (0, True) = New
initialState (x, _) = Connected x

next :: IslandState -> ComparePoints -> ComparePoints -> IslandState
next state p1 p2 | p1 == p2 = state
-- None
next None (0, False) p = initialState p
-- New
next New (0, True) (x, True) = Connected x
-- Connected
next (Connected x) (y, True) (0, True) = Connected x
next (Connected x) (0, True) (y, True) = Connected y
next (Connected x) (y, _) (z, _) = Connected x
next state p1 p2 = error (show state <> show p1 <> show p2)

transform :: Int -> IslandState -> CompareLines -> (Line Int, Int)
transform slot New bs = (transformx slot bs, slot + 1)
transform slot None bs = (transformx 0 bs, slot)
transform slot (Connected x) bs = (transformx x bs, slot)

transformx :: Int -> CompareLines -> Line Int
transformx x = fmap (indicator x . snd)

indicator :: Int -> Bool -> Int
indicator x True = x
indicator _ False = 0
