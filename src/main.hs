{-# LANGUAGE LambdaCase #-}

import IslandSolver (islands)
import Parser (parse)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main =
  getArgs
    >>= \case
      [] -> getContents
      (path : _) -> readFile path
    >>= print . islands . parse
