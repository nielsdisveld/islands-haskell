{-# LANGUAGE LambdaCase #-}

import GHC.Internal.System.Environment (getArgs)
import IslandSolver (islands)
import Parser (parse)

main :: IO ()
main =
  getArgs
    >>= \case
      [] -> getContents
      (path : _) -> readFile path
    >>= print . islands . parse
