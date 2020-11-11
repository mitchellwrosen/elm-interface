module Type
  ( Type (..),
    Constructor (..),
    Deriving (..),
    datatype,
    record,
    typeToHaskell,
  )
where

import Commentable
import Data.Foldable (toList)
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Render

data Type = Type
  { typeComment :: String,
    typeConstructors :: NonEmpty Constructor,
    typeDeriving :: [Deriving],
    typeName :: String
  }

instance Commentable Type where
  comment s ty = ty {typeComment = s}

data Constructor
  = Constructor String [String]
  | Record String (NonEmpty (String, String))

data Deriving
  = Anyclass [String]
  | Newtype [String]
  | Stock [String]
  | Via [String] String

typeToHaskell :: Type -> String
typeToHaskell =
  runRender . renderTypeAsHaskell

renderTypeAsHaskell :: Type -> Render ()
renderTypeAsHaskell Type {typeConstructors, typeDeriving, typeName} = do
  render ("data " ++ typeName ++ " =")
  indent 2 do
    render "\n"
    map renderConstructorAsHaskell (toList typeConstructors) `sepBy` "\n| "
    render "\n"
    map renderDerivingAsHaskell typeDeriving `sepBy` "\n"

renderDerivingAsHaskell :: Deriving -> Render ()
renderDerivingAsHaskell =
  render . \case
    Anyclass types -> "deriving anyclass (" ++ intercalate ", " (sort types) ++ ")"
    Newtype types -> "deriving newtype (" ++ intercalate ", " (sort types) ++ ")"
    Stock types -> "deriving stock (" ++ intercalate ", " (sort types) ++ ")"
    Via types type_ -> "deriving (" ++ intercalate ", " (sort types) ++ ") via (" ++ type_ ++ ")"

renderConstructorAsHaskell :: Constructor -> Render ()
renderConstructorAsHaskell = \case
  Constructor name fields -> render (intercalate " " (name : fields))
  Record name fields -> do
    render (name ++ " {")
    indent 2 do
      render "\n"
      map fieldToHaskell (toList fields) `sepBy` ",\n"
    render "\n}"
    where
      fieldToHaskell :: (String, String) -> Render ()
      fieldToHaskell (k, v) =
        render (k ++ " :: " ++ v)

datatype :: String -> [Constructor] -> [Deriving] -> Type
datatype typeName constructors typeDeriving =
  case NonEmpty.nonEmpty constructors of
    Nothing -> error "no constructors"
    Just typeConstructors ->
      Type
        { typeComment = "",
          typeConstructors,
          typeDeriving,
          typeName
        }

record :: String -> [(String, String)] -> Constructor
record name = \case
  [] -> error "bad record"
  field : fields -> Record name (field :| fields)
