module IslandSolver (islands) where

import Line (Line, zeroes)

islands :: [Line] -> Int
islands = fst . foldr go (0, zeroes)
  where
    go ln (acc, ln0) =
      let zipped = zip ln0 ln
          new = countNew zipped
       in (acc + new, ln)

countNew :: [(Int, Int)] -> Int
countNew = go 0
  where
    go acc [] = acc
    go acc ((_, 0) : xs) = go acc xs
    go acc xs = go (acc + n) rest
      where
        (isNew, rest) = consumeOnes xs
        n = if isNew then 1 else 0

consumeOnes :: [(Int, Int)] -> (Bool, [(Int, Int)])
consumeOnes [] = (True, [])
consumeOnes ((0, 1) : xs) = consumeOnes xs
consumeOnes ((_, 0) : xs) = (True, xs)
consumeOnes ((1, 1) : xs) = (False, dropWhile ((==) 1 . snd) xs)
