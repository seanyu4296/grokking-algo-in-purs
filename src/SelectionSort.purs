module SelectionSort where

import Prelude
import Data.Foldable (minimum)
import Data.List (List(..), delete, (:))
import Data.Maybe (Maybe(..), fromJust)
import Debug.Trace (traceM)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

-- PARTIAL
-- // TODO: ask jv partial functions in haskell
-- // TODO: when to use partial functions
selectionsort :: List Int -> List Int
selectionsort Nil = Nil

selectionsort l = minVal : (selectionsort rest)
  where
  minVal :: Int
  minVal = unsafePartial (fromJust (minimum l))

  rest :: List Int
  rest = delete minVal l

-- TOTAL
selectionsort' :: List Int -> Maybe (List Int)
selectionsort' Nil = Just $ Nil

selectionsort' l = do
  mval <- minimum l
  let
    rest = delete mval l
  restSort <- selectionsort' rest
  pure $ mval : restSort

--
main :: Effect Unit
main = do
  traceM $ selectionsort (1 : 2 : 3 : 5 : 4 : Nil)
  traceM $ selectionsort' (1 : 2 : 3 : 5 : 4 : Nil)


{-
 -- | Notes
 -- | Array (Reading (O(1))) (Insertion (O(n)))
 -- | Linked List (Reading - O(n)) (Insertion - O(1)) -- Constants in Big O????
 -}