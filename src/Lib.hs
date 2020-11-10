module Lib
  ( example,
  )
where

-- import Data.Function
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
-- import qualified Data.List.NonEmpty as NonEmpty

-- import Data.Maybe (catMaybes)

data Type = Type
  { comment :: String,
    constructors :: [Constructor],
    deriving_ :: [Deriving],
    name :: String
  }

data Constructor
  = Constructor String [String]
  | Record String (NonEmpty (String, String))

data Deriving
  = Anyclass [String]
  | Newtype [String]
  | Stock [String]
  | Via [String] String

typeToHaskell :: Type -> String
typeToHaskell Type {constructors, deriving_, name} =
  intercalate
    "\n"
    ( ("data " ++ name) :
      ( "  = "
          ++ intercalate
            "\n  | "
            (map (intercalate "\n    " . constructorToHaskell) constructors)
      ) :
      map (("  " ++) . derivingToHaskell) deriving_
    )

derivingToHaskell :: Deriving -> String
derivingToHaskell = \case
  Anyclass types -> "deriving anyclass (" ++ intercalate ", " (sort types) ++ ")"
  Newtype types -> "deriving newtype (" ++ intercalate ", " (sort types) ++ ")"
  Stock types -> "deriving stock (" ++ intercalate ", " (sort types) ++ ")"
  Via types type_ -> "deriving (" ++ intercalate ", " (sort types) ++ ") via (" ++ type_ ++ ")"

constructorToHaskell :: Constructor -> [String]
constructorToHaskell = \case
  Constructor name fields -> [intercalate " " (name : fields)]
  Record name (field :| fields) ->
    ( (name ++ " {") :
      ("  " ++ fieldToHaskell field) :
      map ((", " ++) . fieldToHaskell) fields
        ++ ["}"]
    )
    where
      fieldToHaskell :: (String, String) -> String
      fieldToHaskell (k, v) =
        k ++ " :: " ++ v

datatype :: String -> [Constructor] -> [Deriving] -> Type
datatype name constructors deriving_ =
  Type
    { comment = "",
      constructors,
      deriving_,
      name
    }

record :: String -> [(String, String)] -> Constructor
record name = \case
  [] -> error "bad record"
  field : fields -> Record name (field :| fields)

example :: IO ()
example = do
  putStrLn $
    typeToHaskell $
      datatype
        "Maybe a"
        [ Constructor "Nothing" [],
          Constructor "Just" ["a"],
          record "What" [("yo", "Text"), ("yoo", "Bool")]
        ]
        [ Stock ["Eq", "Generic", "Show"],
          Anyclass ["Hashable"]
        ]
