module Parser (parse) where

import Data.Char (digitToInt)
import Data.Foldable (traverse_)
import Data.List (break)
import Line (Line)

parse :: String -> [Line]
parse =
  fmap (fmap toInt) . lines
  where
    toInt '0' = 0
    toInt c = 1
