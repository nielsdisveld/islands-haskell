module IslandSolver (islands) where

import Line (Line, zeroes)

-- | Count how many islands on given input
islands :: [Line] -> Int
islands = fst . foldr go (0, zeroes)
  where
    go ln (acc, ln0) =
      let zipped = zip ln0 ln
          new = countNew zipped
       in (acc + new, ln)

-- | Compare line to previous line and count how many new islands are starting
countNew :: [(Int, Int)] -> Int
countNew = go 0
  where
    go acc [] = acc
    go acc ((_, 0) : xs) = go acc xs
    go acc xs = go (acc + n) rest
      where
        (n, rest) = consumeOnes xs

-- | Consumes a contiguous sequence of '1's from the beginning of the line.
--
-- Returns a tuple:
--   (1, rest) if a new island was found
--   (0, rest) if the found island was not new (connected to previous line)
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
consumeOnes :: [(Int, Int)] -> (Int, [(Int, Int)])
consumeOnes [] = (1, [])
consumeOnes ((0, 1) : xs) = consumeOnes xs
consumeOnes ((1, 1) : xs) = (0, dropWhile ((==) 1 . snd) xs)
consumeOnes ((_, 0) : xs) = (1, xs)
