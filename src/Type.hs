module Type
  ( Type (..),
    adt,
    record,
    renderTypeAsHaskell,
  )
where

import Constructor
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Deriving
import Render
import S

data Type = Type
  { typeConstructors :: NonEmpty Constructor,
    typeDeriving :: [Deriving],
    typeName :: S,
    typeVars :: [String]
  }

adt :: S -> [String] -> [(S, [String])] -> [Deriving] -> Type
adt typeName typeVars constructors typeDeriving =
  case NonEmpty.nonEmpty constructors of
    Nothing -> error "no constructors"
    Just typeConstructors ->
      Type
        { typeConstructors = f <$> typeConstructors,
          typeDeriving,
          typeName,
          typeVars
        }
      where
        f :: (S, [String]) -> Constructor
        f (constructorName, constructorFields) =
          Constructor
            { constructorName = constructorName,
              constructorFields = Left constructorFields
            }

record :: S -> [String] -> [(String, String)] -> [Deriving] -> Type
record typeName typeVars fields0 typeDeriving =
  case fields0 of
    [] -> error "bad record"
    field : fields ->
      Type
        { typeConstructors =
            Constructor
              { constructorName = typeName,
                constructorFields = Right (field :| fields)
              }
              :| [],
          typeDeriving,
          typeName,
          typeVars
        }

renderTypeAsHaskell :: Type -> Render ()
renderTypeAsHaskell Type {typeConstructors, typeDeriving, typeName = S typeNameComment typeName} = do
  renderCommentAsHaskell typeNameComment
  render ("data " ++ typeName ++ " =")
  indent 2 do
    render "\n"
    map renderConstructorAsHaskell (toList typeConstructors) `sepBy` " |\n"
    render "\n"
    map renderDerivingAsHaskell typeDeriving `sepBy` "\n"
