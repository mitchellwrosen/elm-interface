module Constructor
  ( Constructor (..),
    renderConstructorAsHaskell,
  )
where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Render
import S

data Constructor = Constructor
  { constructorName :: S,
    constructorFields :: Either [String] (NonEmpty (String, String))
  }

renderConstructorAsHaskell :: Constructor -> Render ()
renderConstructorAsHaskell = \case
  Constructor (S comm name) (Left fields) -> do
    renderCommentAsHaskell comm
    render (intercalate " " (name : fields))
  Constructor (S _comm name) (Right fields) -> do
    render (name ++ " {")
    indent 2 do
      render "\n"
      map fieldToHaskell (toList fields) `sepBy` ",\n"
    render "\n}"
    where
      fieldToHaskell :: (String, String) -> Render ()
      fieldToHaskell (k, v) =
        render (k ++ " :: " ++ v)
