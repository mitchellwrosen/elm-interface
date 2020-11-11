module S
  ( S (..),
    comment,
    renderCommentAsHaskell,
  )
where

import Control.Monad (when)
import Data.String
import Render

data S
  = S String String

instance IsString S where
  fromString =
    S ""

comment :: String -> S -> S
comment c (S _ s) =
  S c s

-- | If the given string is not null, render it as a multi-line comment with a trailing newline.
renderCommentAsHaskell :: String -> Render ()
renderCommentAsHaskell comm =
  when (comm /= "") do
    render "-- | "
    map render (lines comm) `sepBy` "\n-- "
    render "\n"
