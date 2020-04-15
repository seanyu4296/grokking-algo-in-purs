module GrokkingAlgos.DynamicProgramming where

import Prelude
import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array (catMaybes, foldl, updateAt, length, range, snoc, (!!), (:))
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
import Debug.Trace (spy, trace)
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
    [ ("guitar" /\ { weight: 1, price: 1500 })
    , ("stereo" /\ { weight: 4, price: 3000 })
    , ("laptop" /\ { weight: 3, price: 2000 })
    , ("iphone" /\ { weight: 1, price: 2000 })
    , ("keyboard" /\ { weight: 2, price: 2500 })
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

  allItems = spy "ITEMS: " $ allValues <#> fst

  gcf = unsafePartial $ fromJust $ gcd' allWeight

  numberOfColumns = maxWeight / gcf

  y :: Grid
  y =
    spy "GRid: "
      $ foldlWithIndex
          ( \rowIdx grid item ->
              foldlWithIndex
                ( \columnIdx accGrid _ ->
                    let
                      currentItem :: Maybe ItemSpec
                      currentItem = (Map.lookup item itemMap)

                      previousItems :: (Array String)
                      previousItems = getItemsFromGrid accGrid (rowIdx - 1) columnIdx

                      currentGridWeight = getWeightForColumn columnIdx

                      firstcand :: { price :: Int, items :: Array String }
                      firstcand =
                        fromMaybe { price: 0, items: [] }
                          $ do
                              currentI <- currentItem
                              guard (currentI.weight <= currentGridWeight)
                              let
                                remainingWeight = currentGridWeight - currentI.weight

                                columnToGet = getColumnForWeight remainingWeight

                                additionalItems = getItemsFromGrid accGrid (rowIdx - 1) columnToGet

                                allItems = (item : additionalItems)
                              pure $ { price: getTotalPriceItems (item : additionalItems), items: allItems }

                      secondcand :: { price :: Int, items :: Array String }
                      secondcand =
                        let
                          prevPrice = getTotalPriceItems previousItems

                          remainingWeight = currentGridWeight - getTotalWeightItems previousItems

                          columnToGet = getColumnForWeight remainingWeight

                          additionalItems = getItemsFromGrid accGrid (rowIdx - 2) columnToGet

                          allItems = Set.toUnfoldable <<< Set.fromFoldable $ (previousItems <> additionalItems)
                        in
                          { price: getTotalPriceItems allItems, items: allItems }
                    in
                      insertItemsToGrid accGrid rowIdx columnIdx
                        ( if firstcand.price > secondcand.price then
                            firstcand.items
                          else
                            secondcand.items
                        )
                )
                grid
                (range 1 numberOfColumns)
          )
          ([] :: Grid)
          allItems

  -- Helper function
  getWeightForColumn :: Int -> Int
  getWeightForColumn index = gcf * index + 1

  getColumnForWeight :: Int -> Int
  getColumnForWeight weight = (weight / gcf) - 1

  getTotalWeightItems :: Array String -> Int
  getTotalWeightItems items = sum $ catMaybes (items <#> \item -> (Map.lookup item itemMap <#> _.weight))

  getTotalPriceItems :: Array String -> Int
  getTotalPriceItems items = sum $ catMaybes (items <#> \item -> (Map.lookup item itemMap <#> _.price))

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
          newRow <- (updateAt column items oldRow) <|> (pure $ snoc oldRow items)
          (updateAt row newRow grid) <|> (pure $ snoc grid newRow)

--    Array String -> Array (Maybe Int) -> Array Int -> Int
main :: Effect Unit
main = do
  logShow $ solve sampleItems 5
