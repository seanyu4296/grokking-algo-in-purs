module GrokkingAlgos.DynamicProgramming where

import Prelude
import Control.Alt ((<|>))
import Data.Array (catMaybes, foldl, insertAt, length, range, snoc, (!!), (:))
import Data.Foldable (sum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable as Unfoldable
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Partial.Unsafe (unsafePartial)

type ItemSpec
  = { weight :: Int
    , price :: Int
    }

sampleItems :: Map String ItemSpec
sampleItems =
  Map.fromFoldable
    [ ("stereo" /\ { weight: 4, price: 3000 })
    --    , ("laptop" /\ { weight: 3, price: 2000 })
    , ("guitar" /\ { weight: 1, price: 1500 })
    --    , ("iphone" /\ { weight: 1, price: 2000 })
    ]

gcd' :: Array Int -> Maybe Int
gcd' =
  foldl
    ( \acc a -> (acc <#> gcd a) <|> pure a
    )
    Nothing

type Grid
  = Array (Array (Array String))

solve :: Map String ItemSpec -> Int -> Array String
solve itemMap maxWeight =
  fromMaybe []
    ( do
        row <- y !! (length allValues - 1)
        row !! (numberOfColumns - 1)
    )
  where
  allValues :: Array (Tuple String ItemSpec)
  allValues = (Map.toUnfoldable itemMap)

  allWeight = allValues <#> \v -> (snd v).weight

  allItems = allValues <#> fst

  gcf = unsafePartial $ fromJust $ gcd' allWeight

  numberOfColumns = maxWeight / gcf

  y :: Grid
  y =
    foldlWithIndex
      ( \rowIdx grid item ->
          foldlWithIndex
            ( \columnIdx accGrid _ ->
                let
                  currentItemWeight :: Maybe Int
                  currentItemWeight = (Map.lookup item itemMap) <#> _.weight

                  previousItems :: (Array String)
                  previousItems = getItemsFromGrid accGrid (rowIdx - 1) columnIdx

                  totalWeightPrev :: Int
                  totalWeightPrev = getTotalWeightItems previousItems

                  currentGridWeight = getWeightForColumn columnIdx

                  x =
                    fromMaybe []
                      $ do
                          ciw <- currentItemWeight
                          if ciw <= currentGridWeight then
                            let
                              remainingWeight = currentGridWeight - ciw

                              columnToGet = getColumnForWeight remainingWeight

                              additionalItems = getItemsFromGrid accGrid (rowIdx - 1) columnToGet
                            in
                              pure $ insertItemsToGrid accGrid rowIdx columnIdx (item : additionalItems)
                          else
                            pure $ insertItemsToGrid accGrid rowIdx columnIdx previousItems

                  afs = trace (x) (const unit)
                in
                  x
            )
            grid
            (range 1 numberOfColumns)
      )
      ([] :: Grid)
      allItems

  --  limit = getweightForColumn == maxWeight
  -- Helper function
  getWeightForColumn :: Int -> Int
  getWeightForColumn index = gcf * index + 1

  getColumnForWeight :: Int -> Int
  getColumnForWeight weight = weight / gcf

  getTotalWeightItems :: Array String -> Int
  getTotalWeightItems items = sum $ catMaybes (items <#> \item -> (Map.lookup item itemMap <#> _.weight))

  getItemsFromGrid :: Grid -> Int -> Int -> Array String
  getItemsFromGrid grid row column =
    fromMaybe []
      $ do
          row' <- grid !! row
          items <- row' !! column
          pure $ items

  insertItemsToGrid :: Grid -> Int -> Int -> Array String -> Grid
  insertItemsToGrid grid row column items =
    fromMaybe []
      $ do
          oldRow <- (grid !! row) <|> pure []
          --          let
          -- zxzx = trace ("COLUMNIDX:" <> show column) (const unit)
          -- zxzx1 = trace ("ROWIDX:" <> show row) (const unit)
          -- zzzx = trace ("OLDROW:" <> show oldRow) (const unit)
          newRow <- (insertAt column items oldRow) <|> (pure $ snoc oldRow items)
          --          let
          -- zzz = trace ("---" <> show newRow <> "----") (const unit)
          (insertAt row newRow grid) <|> (pure $ snoc grid newRow)

--    Array String -> Array (Maybe Int) -> Array Int -> Int
main :: Effect Unit
main = do
  logShow $ solve sampleItems 4
