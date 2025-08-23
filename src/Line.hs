module Line (Line, zeroes, distinct, (*.), (+.)) where

import Data.Set qualified as Set

type Line = [Int]

zeroes :: Line
zeroes = repeat 0

(*.) :: Line -> Line -> Line
(*.) = zipWith (*)

(+.) :: Line -> Line -> Line
(+.) = zipWith (+)

distinct :: Line -> Int
distinct = Set.size . Set.delete 0 . Set.fromList
