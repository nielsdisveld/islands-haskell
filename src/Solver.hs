module Solver (solve) where

import Distribution.Utils.Generic (fstOf3)
import Line (Line, zeroes)

-- | Type alias for the island count.
type Count = Int

-- | Marker used to identify different islands.
type Marker = Int

{- | Represents a pair of a marker from the previous line and a Boolean indicating
current land presence.
Used to track how land connects between rows.
Values can be categorized by  : or . or   or · and these characters will be used
to illustrate which situations are handled.
-}
type PairedCell = (Marker, Bool)

{- | A line of `ComparePoints`, where each point pairs the current cell
with the corresponding cell directly above it.

Think of it as:

>    Previous row:  [ 0, 0, 1, 1, 0 ]
>    Current row:   [ 1, 0, 1, 1, 1 ]
>    CompareLines:  [(0,1),(0,0),(1,1),(1,1),(0,1)]

Each (Marker, Bool) = (marker from previous line, land in current line)
-}
type PairedLine = Line PairedCell

-- | Describes the state of a land segment as it's being evaluated.
data IslandState = None | New | Connected Marker
    deriving (Eq, Show)

{- Count how many islands on given grid.
Assumes all lines are of equal length
-}
solve :: [Line Bool] -> Count
solve = fstOf3 . foldr go (0, 1, zeroes)
  where
    go :: Line Bool -> (Count, Marker, Line Marker) -> (Count, Marker, Line Marker)
    go ln (count, marker, prev) =
        let paired = zip prev ln
         in processLine count marker paired

{- Compare line to previous line and count how many new islands are starting or merging.
Also return a transformed version of current line where the islands are marked
-}
processLine :: Count -> Marker -> PairedLine -> (Count, Marker, Line Marker)
processLine count marker paired =
    case splitConnected paired of
        ([], []) -> (count, marker, [])
        (segment, rest) ->
            let initial = initialState (head segment)
                (newCount, state) = processConnected count initial segment
                marked = mark marker state segment
                newMarker = increaseMarker state marker
                (finalCount, finalMarker, restMarked) = processLine newCount newMarker rest
             in (finalCount, finalMarker, marked ++ restMarked)

-- Determine the state of a connected segment.
processConnected :: Count -> IslandState -> PairedLine -> (Count, IslandState)
processConnected count state (p1 : p2 : rest) =
    case (state, next state p1 p2) of
        -- First case is when the current line connects 2 different islands from the previous line
        -- with each other.
        (Connected x, Connected y) | x /= y -> processConnected (count - 1) state (p2 : rest)
        (_, nextState) -> processConnected count nextState (p2 : rest)
processConnected count New _ = (count + 1, New) -- Found a new island (not connected to previous line)
processConnected count state _ = (count, state)

{- Splits a `PairedLine` into a connected region and the rest.

Schematically:

>  :.. : .:··:
>   ↑    ↑
>  (x, rest)
-}
splitConnected :: PairedLine -> (PairedLine, PairedLine)
splitConnected [] = ([], [])
splitConnected [x] = ([x], [])
splitConnected (x : y : xs) =
    if isConnected x y
        then
            let (land, rest) = splitConnected (y : xs)
             in (x : land, rest)
        else ([x], y : xs)

-- Determines if two adjacent cells in a line are connected as land
isConnected :: PairedCell -> PairedCell -> Bool
isConnected p1 p2 =
    case (p1, p2) of
        ((0, False), _) -> True -- By definition we called this connected
        (_, (0, False)) -> False -- End of land
        _ | p1 == p2 -> True -- .. or :: or ··
        ((_, True), (_, True)) -> True -- :. or .:
        ((0, True), (_, False)) -> False -- .·
        ((_, False), (0, True)) -> False -- ·.
        ((x, _), (y, _)) | x == y -> True -- ·: or :·

-- Determines the initial state of a PairedCell
initialState :: PairedCell -> IslandState
initialState (0, False) = None
initialState (0, True) = New
initialState (x, _) = Connected x

-- Determines the state change when traversing two PairedCells
next :: IslandState -> PairedCell -> PairedCell -> IslandState
-- Islandstate is preserved when traversing identical PairedCells
next state p1 p2 | p1 == p2 = state -- .. or :: or ··
next None (0, False) p = initialState p
next New (0, True) (x, True) = Connected x -- .:
next (Connected x) (y, True) (0, True) = Connected x -- :.
next (Connected x) (0, True) (y, True) = Connected y -- .: This situation possibly merges 2 islands
next (Connected x) (y, _) (z, _) = Connected x -- ·: or :·

-- Mark the boolean line with the current marker (True -> marker, False -> 0)
mark :: Marker -> IslandState -> PairedLine -> Line Marker
mark marker New bs = markBy marker bs
mark marker None bs = markBy 0 bs
mark marker (Connected x) bs = markBy x bs

-- Apply a specific marker to all land positions
markBy :: Marker -> PairedLine -> Line Marker
markBy x = fmap (indicator x . snd)

-- Indicator function that marks when True
indicator :: Marker -> Bool -> Marker
indicator x True = x
indicator _ False = 0

-- Increase marker only if the current state introduced a new island
increaseMarker :: IslandState -> Marker -> Marker
increaseMarker New marker = marker + 1
increaseMarker _ marker = marker
