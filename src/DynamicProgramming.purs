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
import Data.Set as Set
import Data.String as Str
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
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

type KnapsackGrid
  = Grid (Array String)

solve :: Map String ItemSpec -> Int -> Array String
solve itemMap maxWeight = getItemsFromGrid grid (length allValues - 1) (numberOfColumns - 1)
  where
  allValues :: Array (Tuple String ItemSpec)
  allValues = (Map.toUnfoldable itemMap)

  allWeight = allValues <#> \v -> (snd v).weight

  allItems = allValues <#> fst

  gcf = unsafePartial $ fromJust $ gcd' allWeight

  numberOfColumns = maxWeight / gcf

  grid :: KnapsackGrid
  grid =
    foldlWithIndex
      ( \rowIdx accGrid item ->
          foldlWithIndex
            ( \columnIdx accGrid2 _ ->
                let
                  currentItem :: Maybe ItemSpec
                  currentItem = (Map.lookup item itemMap)

                  previousItems :: (Array String)
                  previousItems = getItemsFromGrid accGrid2 (rowIdx - 1) columnIdx

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

                            additionalItems = getItemsFromGrid accGrid2 (rowIdx - 1) columnToGet

                            allItems = (item : additionalItems)
                          pure $ { price: getTotalPriceItems (item : additionalItems), items: allItems }

                  secondcand :: { price :: Int, items :: Array String }
                  secondcand =
                    let
                      prevPrice = getTotalPriceItems previousItems

                      remainingWeight = currentGridWeight - getTotalWeightItems previousItems

                      columnToGet = getColumnForWeight remainingWeight

                      additionalItems = getItemsFromGrid accGrid2 (rowIdx - 2) columnToGet

                      allItems = Set.toUnfoldable <<< Set.fromFoldable $ (previousItems <> additionalItems)
                    in
                      { price: getTotalPriceItems allItems, items: allItems }
                in
                  insertValToGrid accGrid2 rowIdx columnIdx
                    ( if firstcand.price > secondcand.price then
                        firstcand.items
                      else
                        secondcand.items
                    )
            )
            accGrid
            (range 1 numberOfColumns)
      )
      ([] :: KnapsackGrid)
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

  getItemsFromGrid :: KnapsackGrid -> Int -> Int -> Array String
  getItemsFromGrid g x y = fromMaybe [] $ getCellFromGrid g x y

-- Longest subsequence
type LCSGrid
  = Grid Int

lcs :: String -> String -> Int
lcs word1 word2 = fromMaybe 0 getBtmRightCell
  where
  getBtmRightCell = getCellFromGrid grid (longestWordL - 1) (longestWordL - 1)

  longestWordL = max (Str.length word1) (Str.length word2)

  word1Chars = toCharArray word1

  word2Chars = toCharArray word2

  grid =
    foldlWithIndex
      ( \rowIdx accGrid row ->
          foldlWithIndex
            ( \colIdx accGrid2 cell ->
                let
                  word1Char = word1Chars !! rowIdx

                  word2Char = word2Chars !! colIdx

                  insertval = insertValToGrid accGrid2 rowIdx colIdx
                in
                  insertval
                    $ if word1Char == word2Char then
                        fromMaybe 0 (getLeftDiagCell accGrid2 rowIdx colIdx) + 1
                      else
                        fromMaybe 0 $ max (getTopAdjCell accGrid2 rowIdx colIdx) (getLeftAdjCell accGrid2 rowIdx colIdx)
            )
            accGrid
            (range 1 longestWordL)
      )
      ([] :: LCSGrid)
      (range 1 longestWordL)

  getLeftDiagCell :: forall a. Grid a -> Int -> Int -> Maybe a
  getLeftDiagCell g x y = getCellFromGrid g (x - 1) (y - 1)

  getTopAdjCell :: forall a. Grid a -> Int -> Int -> Maybe a
  getTopAdjCell g x y = getCellFromGrid g (x - 1) y

  getLeftAdjCell :: forall a. Grid a -> Int -> Int -> Maybe a
  getLeftAdjCell g x y = getCellFromGrid g x (y - 1)

-- Helper Functions For Grid
type Grid a
  = Array (Array a)

getCellFromGrid :: forall a. Grid a -> Int -> Int -> Maybe a
getCellFromGrid grid row column = do
  row' <- grid !! row
  items <- row' !! column
  pure $ items

insertValToGrid :: forall a. Grid a -> Int -> Int -> a -> Grid a
insertValToGrid grid row column newVal =
  fromMaybe []
    $ do
        oldRow <- (grid !! row) <|> pure []
        newRow <- (updateAt column newVal oldRow) <|> (pure $ snoc oldRow newVal)
        (updateAt row newRow grid) <|> (pure $ snoc grid newRow)

-- Main
main :: Effect Unit
main = do
  logShow $ solve sampleItems 5
  logShow $ lcs "sean" "efasdan"
