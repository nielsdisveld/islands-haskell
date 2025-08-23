module Parser (parse) where

import Line (Line)

parse :: String -> [Line]
parse =
  fmap (fmap toInt) . lines
  where
    toInt '0' = 0
    toInt c = 1
