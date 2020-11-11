module Lib
  ( example,
  )
where

import Commentable
import Constructor
import Deriving
import Render
import Type

example :: IO ()
example = do
  putType $
    comment "the maybe type" $
      adt
        "Maybe a"
        [ ("Nothing", []),
          ("Just", ["a"])
        ]
        [ Stock ["Eq", "Generic", "Show"],
          Anyclass ["Hashable"]
        ]

  putType $
    comment "A non-empty text type" $
      adt
        "Text1"
        [("Text1", ["Text"])]
        [ Stock ["Eq", "Generic", "Ord", "Show"],
          Newtype ["Hashable", "FromField", "ToField", "ToString", "ToText"],
          Anyclass ["SOP.HasDatatypeInfo", "SOP.Generic", "ToJSON"]
        ]

putType :: Type -> IO ()
putType =
  putStrLn . runRender . renderTypeAsHaskell
