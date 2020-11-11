module Render
  ( Render,
    render,
    indent,
    runRender,
  )
where

import Data.Function

newtype Render a
  = Render (Int -> Dlist Char -> (Dlist Char, a))
  deriving stock (Functor)

instance Applicative Render where
  pure x = Render \_ s -> (s, x)
  Render sf <*> Render sx =
    Render \n s0 ->
      case sf n s0 of
        (s1, f) ->
          case sx n s1 of
            (s2, x) ->
              case f x of
                y -> (s2, y)

instance Monad Render where
  Render sx >>= f =
    Render \n s0 ->
      case sx n s0 of
        (s1, x) ->
          case f x of
            Render sy -> sy n s1

runRender :: Render () -> String
runRender (Render f) =
  case f 0 mempty of
    (x, ()) -> runDlist x

render :: String -> Render ()
render s =
  Render \n xs ->
    case xs <> dlist (format n s) of
      ys -> (ys, ())
  where
    format :: Int -> [Char] -> [Char]
    format n =
      fix \loop ->
        \case
          [] -> []
          '\n' : xs -> '\n' : spaces ++ loop xs
          x : xs -> x : loop xs
      where
        spaces :: [Char]
        spaces =
          replicate n ' '

indent :: Int -> Render a -> Render a
indent n (Render f) =
  Render \m -> f (m + n)

-- Difference list

newtype Dlist a
  = Dlist ([a] -> [a])

instance Monoid (Dlist a) where
  mempty = Dlist id
  mappend = (<>)

instance Semigroup (Dlist a) where
  Dlist f <> Dlist g =
    Dlist (f . g)

dlist :: [a] -> Dlist a
dlist xs =
  Dlist \ys -> xs ++ ys

runDlist :: Dlist a -> [a]
runDlist (Dlist f) =
  f []
