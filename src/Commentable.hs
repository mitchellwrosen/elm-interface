module Commentable
  ( Commentable (..),
  )
where

class Commentable a where
  comment :: String -> a -> a
