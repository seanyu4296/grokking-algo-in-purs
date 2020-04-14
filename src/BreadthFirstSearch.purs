module GrokkingAlgos.BreadthFirstSearch where

import Prelude
import Data.Array (elem, (:))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)

-- | Steps:
-- | Model the problem as graph
-- | Finding the shortest path
-- | Use breadth-first search
-- | Graph - nodes, edges, neighbors
-- | Breadth first search answers two questions -  is there a path from node a to node b, what is the shortest path
-- | Breadth-first search takes O(number of people + number of edges), and it’s more commonly written as O(V+E) (V for number of vertices, E for number of edges).
sampleGraph :: Map String (Array String)
sampleGraph =
  Map.fromFoldable
    [ ("you" /\ [ "alice", "bob", "claire" ])
    , ("bob" /\ [ "anuj", "peggy" ])
    , ("alice" /\ [ "peggy" ])
    , ("claire" /\ [ "manny", "jonny" ])
    , ("anuj" /\ [])
    , ("peggy" /\ [])
    , ("manny" /\ [])
    , ("jonny" /\ [])
    ]

-- check first degree
-- add if not there yet
-- keep list of checked already
bfs :: String -> Map String (Array String) -> (String -> Boolean) -> Maybe String
bfs start graph checker = do
  connections <- Map.lookup start graph
  go connections []
  where
  go queue searched = case Array.uncons queue of
    Just { head, tail }
      | checker head -> Just head
      | head `elem` searched -> go tail searched
      | otherwise -> go (tail <> fromMaybe [] (Map.lookup head graph)) (head : searched)
    Nothing -> Nothing

main :: Effect Unit
main = do
  logShow $ bfs "you" sampleGraph (\s -> s == "you")
