module Constructor
  ( Constructor (..),
    renderConstructorAsHaskell,
  )
where

import Commentable
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Render
import Control.Monad (when)

data Constructor = Constructor
  { constructorComment :: String,
    constructorName :: String,
    constructorFields :: Either [String] (NonEmpty (String, String))
  }

instance Commentable Constructor where
  comment :: String -> Constructor -> Constructor
  comment s x =
    x {constructorComment = s}

renderConstructorAsHaskell :: Constructor -> Render ()
renderConstructorAsHaskell = \case
  Constructor comm name (Left fields) -> do
    when (comm /= "") do
      render "-- | "
      map render (lines comm) `sepBy` "\n-- "
    render (intercalate " " (name : fields))
  Constructor comm name (Right fields) -> do
    render (name ++ " {")
    indent 2 do
      render "\n"
      map fieldToHaskell (toList fields) `sepBy` ",\n"
    render "\n}"
    where
      fieldToHaskell :: (String, String) -> Render ()
      fieldToHaskell (k, v) =
        render (k ++ " :: " ++ v)
