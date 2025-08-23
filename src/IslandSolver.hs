module IslandSolver (islands) where

type Line = [Int]

zeroes :: Line
zeroes = repeat 0

islands :: [Line] -> Int
islands = fst . foldr go (0, zeroes)
  where
    go ln (acc, ln0) =
      let new = countNew ln0 ln
       in (acc + new, ln)

countNew :: Line -> Line -> Int
countNew ln1 ln2 = go 0 zipped
  where
    zipped = zip ln1 ln2
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
