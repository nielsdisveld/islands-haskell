module Solver (solve) where

import Line (Line, zeroes)

-- | Helper type that represents the previous line and the current line
-- that is to be evaluated. The first coordinate of a point represents entry on
-- previous line.
type TwoLines = [(Bool, Bool)]

-- | Count how many islands on given input.
solve :: [Line] -> Int
solve = fst . foldr go (0, zeroes)
  where
    go ln (acc, prev) =
      let twoLines = zip prev ln
          new = countNew twoLines
       in (acc + new, ln)

-- | Compare line to previous line and count how many new islands are starting.
countNew :: TwoLines -> Int
countNew = go 0
  where
    go acc ln = case consumeZeroes ln of
      [] -> acc
      xs ->
        let (n, rest) = consumeOnes xs
         in go (acc + n) rest

-- | Consume contiguous sequence of '0's.
consumeZeroes :: TwoLines -> TwoLines
consumeZeroes = dropWhile ((==) False . snd)

-- | Consumes a contiguous sequence of '1's.
--
-- Returns a tuple:
--   (1, rest) if a new island was found
--   (0, rest) if the found island was not new (connected to an island on previous line)
--
-- 'rest' is the remaining portion of the line after consuming the contiguous '1's.
--
-- Example (Note: argument is a zip of 2 lines):
--
--               "000111"        "111"
--   consumeOnes "111001" == (1, "001")
--
--               "10011"         "0011"
--   consumeOnes "10111"  == (0, "0111")
consumeOnes :: TwoLines -> (Int, TwoLines)
consumeOnes [] = (1, [])
consumeOnes (p : ps) = case p of
  (False, True) -> consumeOnes ps -- Entry above was not an island.
  (True, True) -> (0, dropWhile ((==) True . snd) ps) -- Entry above is an island so current is not a new one.
  (_, False) -> (1, ps) -- End of island which was new.
