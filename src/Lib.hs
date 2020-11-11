module Lib
  ( example,
  )
where

import Commentable
import Type

example :: IO ()
example = do
  putStrLn $
    typeToHaskell $
      comment "the maybe type" $
        datatype
          "Maybe a"
          [ Constructor "Nothing" [],
            Constructor "Just" ["a"],
            record "What" [("yo", "Text"), ("yoo", "Bool")]
          ]
          [ Stock ["Eq", "Generic", "Show"],
            Anyclass ["Hashable"]
          ]

  putStrLn $
    typeToHaskell $
      comment "A non-empty text type" $
        datatype
          "Text1"
          [Constructor "Text1" ["Text"]]
          [ Stock ["Eq", "Generic", "Ord", "Show"],
            Newtype ["Hashable", "FromField", "ToField", "ToString", "ToText"],
            Anyclass ["SOP.HasDatatypeInfo", "SOP.Generic", "ToJSON"]
          ]
