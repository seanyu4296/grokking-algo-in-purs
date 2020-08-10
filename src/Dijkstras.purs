module GrokkingAlgos.Dijkstras where

import Prelude
import Control.Alt ((<|>))
import Data.Array (snoc)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class.Console (logShow)

{- dijkstra :: (Graph g, Ord a) => g a -> Vertex a -> (Distances a, Prevs a)
dijkstra graph source =
  let
    q = vertices graph
    dist = M.singleton source 0
  in -}
type Graph
  = Map String (Map String Int)

type CostTable
  = Map String (Maybe Int)

type ParentsTable
  = Map String (Maybe String)

type Result
  = { path :: Array String
    , cost :: Int
    }

type Result'
  = { costTable :: CostTable
    , parentsTable :: ParentsTable
    }

samplegraph :: Graph
samplegraph =
  Map.fromFoldable
    [ ("start" /\ Map.fromFoldable [ ("a" /\ 6), ("b" /\ 2) ])
    , ("a" /\ Map.fromFoldable [ ("fin" /\ 1) ])
    , ("b" /\ Map.fromFoldable [ ("a" /\ 3), ("fin" /\ 5) ])
    , ("fin" /\ Map.empty)
    ]

djikstra :: Graph -> String -> String -> Maybe Result
djikstra graph start end = go init.costTable init.parentsTable Set.empty
  where
  go :: CostTable -> ParentsTable -> Set String -> Maybe Result
  go costTable parentsTable processed =
    let
      lowestCostNodeM = findLowestCostNode costTable processed

      goPath' cnodeM acc = case cnodeM of
        Just cnode -> do
          pnode <- join $ Map.lookup cnode parentsTable
          goPath' (join $ Map.lookup pnode parentsTable) $ snoc acc pnode
        Nothing -> pure acc
    in
      case lowestCostNodeM of
        Nothing -> do
          cost <- join $ Map.lookup end costTable
          path <- goPath' (Just end) []
          pure { cost, path }
        Just lowestCostNode ->
          let
            neighborsM = Map.lookup lowestCostNode graph
          in
            do
              new <-
                neighborsM
                  <#> ( \neighborss ->
                        foldl
                          ( \acc neighbor ->
                              let
                                costNeigh = fromMaybe 0 $ Map.lookup neighbor neighborss

                                currCost = join $ Map.lookup neighbor costTable

                                lowestCostNodeCost = fromMaybe 0 $ join $ Map.lookup lowestCostNode costTable

                                newCost = lowestCostNodeCost + costNeigh

                                mkNew nC n lcn =
                                  { costTable: Map.insert n (Just nC) acc.costTable
                                  , parentsTable: Map.insert n (Just lcn) acc.parentsTable
                                  }
                              in
                                case currCost of
                                  Just cCost ->
                                    if newCost < cCost then
                                      mkNew newCost neighbor lowestCostNode
                                    else
                                      acc
                                  Nothing -> mkNew newCost neighbor lowestCostNode
                          )
                          { costTable, parentsTable }
                          (Map.keys neighborss)
                    )
              let
                newProcessed = Set.insert lowestCostNode processed
              go new.costTable new.parentsTable newProcessed

  findLowestCostNode :: CostTable -> Set String -> Maybe String
  findLowestCostNode costTable processed =
    foldl
      ( \(acc :: Maybe String) key ->
          let
            currValM = Map.lookup key costTable

            accValM = do
              accKey <- acc
              Map.lookup accKey costTable
          in
            case accValM of
              Just (Just accVal) -> case currValM of
                Just (Just currVal) ->
                  if currVal < accVal then
                    pure $ key
                  else
                    acc
                _ -> acc
              _ -> currValM $> key
      )
      Nothing
      (Set.difference (Map.keys costTable) processed)

  init :: { costTable :: CostTable, parentsTable :: ParentsTable }
  init =
    let
      connectedNodes :: Maybe (Map String Int)
      connectedNodes = Map.lookup start graph

      keys :: Array String
      keys = Set.toUnfoldable $ Set.filter (notEq start) (Map.keys graph)
    in
      foldl
        ( \acc key ->
            let
              res = do
                costMap <- connectedNodes
                cost <- Map.lookup key costMap
                pure $ { start, cost }
            in
              { parentsTable:
                  Map.insert key
                    ( res <#> _.start
                    )
                    acc.parentsTable
              , costTable:
                  Map.insert key
                    ( res <#> _.cost
                    )
                    acc.costTable
              }
        )
        { costTable: Map.empty, parentsTable: Map.empty }
        keys

main :: Effect Unit
main = do
  logShow $ djikstra samplegraph "start" "fin"
