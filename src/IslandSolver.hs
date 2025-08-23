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
        (isNew, rest) = consumeOnes xs
        n = if isNew then 1 else 0

-- | Consume one '1' or more from the line while also checking if it is starting
-- a new island
consumeOnes :: [(Int, Int)] -> (Bool, [(Int, Int)])
consumeOnes [] = (True, [])
consumeOnes ((0, 1) : xs) = consumeOnes xs
consumeOnes ((1, 1) : xs) = (False, dropWhile ((==) 1 . snd) xs)
consumeOnes ((_, 0) : xs) = (True, xs)
