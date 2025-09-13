{-# LANGUAGE LambdaCase #-}

import Line (parse)
import Solver (solve)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main =
  getArgs
    >>= \case
      [] -> getContents
      (path : _) -> readFile path
    >>= print . solve . parse
