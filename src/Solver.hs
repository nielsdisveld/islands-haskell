module Solver (solve) where

import Distribution.Utils.Generic (fstOf3)
import Line (Line, zeroes)

-- | Represents island count
type Count = Int

-- | Represents the current land marker
type Marker = Int

-- | A tuple that represents the current point (Bool) compared to the marked previous point
type ComparePoints = (Int, Bool)

-- | List of ComparePoint which can be seen as a 2-wide grid
type CompareLines = Line ComparePoints

data IslandState = None | New | Connected Int
  deriving (Eq, Show)

-- | Count how many islands on given input.
-- Assumed is that all line are of equal length
solve :: [Line Bool] -> Count
solve = fstOf3 . foldr go (0, 1, zeroes)
  where
    go :: Line Bool -> (Count, Marker, Line Int) -> (Count, Marker, Line Int)
    go ln (count, marker, prev) =
      let twoLines = zip prev ln
       in countNew count marker twoLines

-- | Compare line to previous line and count how many new islands are starting or merging.
-- Also return a transformed version of current line where the islands are labeled
countNew :: Count -> Marker -> CompareLines -> (Count, Marker, Line Int)
countNew count marker lns =
  case splitConnected lns of
    ([], []) -> (count, marker, [])
    (land, rest) ->
      let initial = initialState (head land)
          (newCount, state) = exploreConnected count initial land
          transformed = mark marker state land
          newMarker = increaseMarker state marker
          (finalCount, finalMarker, finalTransformed) = countNew newCount newMarker rest
       in (finalCount, finalMarker, transformed ++ finalTransformed)

-- | Determine the state of a connected 2-wide grid
exploreConnected :: Count -> IslandState -> CompareLines -> (Count, IslandState)
exploreConnected count state (p1 : p2 : rest) =
  case (state, next state p1 p2) of
    (Connected x, Connected y) | x /= y -> exploreConnected (count - 1) state (p2 : rest)
    (_, nextState) -> exploreConnected count nextState (p2 : rest)
exploreConnected count New _ = (count + 1, New) -- Found a new island (not connected to previous line)
exploreConnected count state _ = (count, state)

-- | Break two compare lines so that we split of a connected 2-wide grid from the rest
splitConnected :: CompareLines -> (CompareLines, CompareLines)
splitConnected [] = ([], [])
splitConnected [x] = ([x], [])
splitConnected (x : y : xs) =
  if isConnected x y
    then
      let (land, rest) = splitConnected (y : xs)
       in (x : land, rest)
    else ([x], y : xs)

-- | Determines if a 2x2 grid is connected
isConnected :: ComparePoints -> ComparePoints -> Bool
isConnected p1 p2 =
  case (p1, p2) of
    ((0, False), _) -> True -- By definition we called this connected
    (_, (0, False)) -> False -- End of land
    _ | p1 == p2 -> True -- .. or :: or ··
    ((_, True), (_, True)) -> True -- :. or .:
    ((0, True), (_, False)) -> False -- .·
    ((_, False), (0, True)) -> False -- ·.
    ((x, _), (y, _)) | x == y -> True -- ·: or :·

-- | Returns the island state of a 1x2 grid
initialState :: ComparePoints -> IslandState
initialState (0, False) = None
initialState (0, True) = New
initialState (x, _) = Connected x

-- | Determines the state change when traversing a 2x2 grid
next :: IslandState -> ComparePoints -> ComparePoints -> IslandState
-- Islandstate is preserved when traversing identical ComparePoints
next state p1 p2 | p1 == p2 = state -- .. or :: or ··
-- None
next None (0, False) p = initialState p
-- New
next New (0, True) (x, True) = Connected x -- .:
-- Connected
next (Connected x) (y, True) (0, True) = Connected x -- :.
next (Connected x) (0, True) (y, True) = Connected y -- .: This situation possibly merges 2 islands
next (Connected x) (y, _) (z, _) = Connected x -- ·: or :·

-- | Mark the boolean line with the current marker (True -> marker, False -> 0)
mark :: Marker -> IslandState -> CompareLines -> Line Int
mark marker New bs = markBy marker bs
mark marker None bs = markBy 0 bs
mark marker (Connected x) bs = markBy x bs

markBy :: Marker -> CompareLines -> Line Int
markBy x = fmap (indicator x . snd)

indicator :: Marker -> Bool -> Int
indicator x True = x
indicator _ False = 0

increaseMarker :: IslandState -> Marker -> Marker
increaseMarker New marker = marker + 1
increaseMarker _ marker = marker
