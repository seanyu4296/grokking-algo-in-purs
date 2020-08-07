module GrokkingAlgos.Dijkstras where

import Data.Map (Map)

 {- dijkstra :: (Graph g, Ord a) => g a -> Vertex a -> (Distances a, Prevs a)
dijkstra graph source =
  let
    q = vertices graph
    dist = M.singleton source 0
  in -}

type Graph = Map String (Map String Int)
type CostTable = Map String (Maybe Int)
type ParentsTable = Map String String
type Result =
  { path :: Array String
  , cost :: Int
  }

-- // TODO: Create sample data


djikstra :: Graph -> String -> Maybe Result
djikstra graph start =
  where
  go :: CostTable -> ParentsTable -> Array String -> Maybe Result
  go costs parents processed =
    -- this part has to be one big do Maybe recursion
    -- if it cannot find a connection to "fin" then Nothing
    -- if isNothing findLowestCostNode then return result

  -- // TODO: use first node and get connecting nodes for initial table
  initCosts :: CostTable
  initCosts =

  initParents :: ParentsTable
  initParents =

  findLowestCostNode :: CostTable -> Array String -> Maybe String
  findLowestCostNode costs processed =