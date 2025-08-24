module Parser (parse) where

import Line (Line)

parse :: String -> [Line]
parse =
  fmap (fmap toInt) . lines
  where
    toInt '0' = False
    toInt c = True
