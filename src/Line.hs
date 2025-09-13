module Line (Line, zeroes, parse) where

type Line a = [a]

zeroes :: Line Int
zeroes = repeat 0

parse :: String -> [Line Bool]
parse =
  fmap (fmap toInt) . lines
  where
    toInt '0' = False
    toInt c = True
