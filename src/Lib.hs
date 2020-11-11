module Lib
  ( example,
  )
where

import Data.Function
import Deriving
import Render
import S
import Type

example :: IO ()
example = do
  putType $
    adt
      "Maybe"
      ["a"]
      [ ("Nothing" & comment "no data here", []),
        ("Just" & comment "one 'a' in here", ["a"])
      ]
      [ Stock ["Eq", "Generic", "Show"],
        Anyclass ["Hashable"]
      ]

  putStrLn ""

  putType $
    adt
      ("Text1" & comment "A non-empty text type")
      []
      [("Text1", ["Text"])]
      [ Stock ["Eq", "Generic", "Ord", "Show"],
        Newtype ["Hashable", "FromField", "ToField", "ToString", "ToText"],
        Anyclass ["SOP.HasDatatypeInfo", "SOP.Generic", "ToJSON"]
      ]

  putStrLn ""

  putType $
    record
      ("User" & comment "the user type")
      []
      [("name", "String"), ("age", "Int")]
      [Stock ["Show", "Generic"]]

putType :: Type -> IO ()
putType =
  putStrLn . runRender . renderTypeAsHaskell
