module Parser (parse) where

import Data.Char (digitToInt)
import Data.Foldable (traverse_)
import Data.List (break)
import Line (Line)

parse :: String -> [Line]
parse =
  fmap (fmap toInt) . splitOn '\n'
  where
    toInt '0' = 0
    toInt c = 1

splitOn _ [] = []
splitOn x xs = case rest of
  [] -> [ys]
  _ : zs -> ys : splitOn x zs
  where
    (ys, rest) = break (== x) xs
