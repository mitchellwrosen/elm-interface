module Deriving
  ( Deriving (..),
    renderDerivingAsHaskell,
  )
where

import Data.List (intercalate, sort)
import Render

data Deriving
  = Anyclass [String]
  | Newtype [String]
  | Stock [String]
  | Via [String] String

renderDerivingAsHaskell :: Deriving -> Render ()
renderDerivingAsHaskell =
  render . \case
    Anyclass types -> "deriving anyclass (" ++ intercalate ", " (sort types) ++ ")"
    Newtype types -> "deriving newtype (" ++ intercalate ", " (sort types) ++ ")"
    Stock types -> "deriving stock (" ++ intercalate ", " (sort types) ++ ")"
    Via types type_ -> "deriving (" ++ intercalate ", " (sort types) ++ ") via (" ++ type_ ++ ")"
