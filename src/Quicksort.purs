module GrokkingAlgos.Quicksort where

import Prelude
import Data.Array (partition, (:))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Partial.Unsafe (unsafePartial)

-- | Worst case for Quicksort O (n^2)
-- | Average case for Quicksort O (n log n)
-- | If two algos have diff big o times, constant doesn't matter
-- | Worst case, best case
quicksort :: Array Int -> Maybe (Array Int)
quicksort [] = Just []

quicksort arr = do
  { head, tail } <- Array.uncons arr
  case tail of
    [] -> pure [ head ]
    _ ->
      let
        { yes: less, no: greater } = partition (_ <= head) tail
      in
        ado
          l <- (quicksort less)
          g <- (quicksort greater)
          in l <> [ head ] <> g

quicksort' :: Array Int -> Array Int
quicksort' [] = []

quicksort' arr =
  let
    { head, tail } = unsafePartial $ fromJust $ Array.uncons arr
  in
    case tail of
      [] -> [ head ]
      _ ->
        let
          { yes: less, no: greater } = partition (_ <= head) tail
        in
          (quicksort' less) <> [ head ] <> (quicksort' greater)

main :: Effect Unit
main = do
  let
    x = [ 1, 5, 23, 4, 5, 1, 235, 1, 2, 15, 136, 1212, 2365, 263, 124, 2346, 235616 ]
  logShow $ quicksort x
  logShow $ quicksort' x
