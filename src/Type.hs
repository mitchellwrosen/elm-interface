module Type
  ( Type (..),
    adt,
    record,
    renderTypeAsHaskell,
  )
where

import Commentable
import Constructor
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Deriving
import Render

data Type = Type
  { typeComment :: String,
    typeConstructors :: NonEmpty Constructor,
    typeDeriving :: [Deriving],
    typeName :: String
  }

instance Commentable Type where
  comment :: String -> Type -> Type
  comment s x =
    x {typeComment = s}

adt :: String -> [(String, [String])] -> [Deriving] -> Type
adt typeName constructors typeDeriving =
  case NonEmpty.nonEmpty constructors of
    Nothing -> error "no constructors"
    Just typeConstructors ->
      Type
        { typeComment = "",
          typeConstructors = f <$> typeConstructors,
          typeDeriving,
          typeName
        }
      where
        f :: (String, [String]) -> Constructor
        f (constructorName, constructorFields) =
          Constructor
            { constructorComment = "",
              constructorName,
              constructorFields = Left constructorFields
            }

record :: String -> [(String, String)] -> [Deriving] -> Type
record typeName fields0 typeDeriving =
  case fields0 of
    [] -> error "bad record"
    field : fields ->
      Type
        { typeComment = "",
          typeConstructors =
            Constructor
              { constructorComment = "",
                constructorName = typeName,
                constructorFields = Right (field :| fields)
              }
              :| [],
          typeDeriving,
          typeName
        }

renderTypeAsHaskell :: Type -> Render ()
renderTypeAsHaskell Type {typeConstructors, typeDeriving, typeName} = do
  render ("data " ++ typeName ++ " =")
  indent 2 do
    render "\n"
    map renderConstructorAsHaskell (toList typeConstructors) `sepBy` "\n| "
    render "\n"
    map renderDerivingAsHaskell typeDeriving `sepBy` "\n"
