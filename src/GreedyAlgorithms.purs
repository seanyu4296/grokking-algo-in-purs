module GrokkingAlgos.GreedyAlgorithms where

import Prelude
import Data.Array (foldl, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Class.Console (logShow)


-- Stations problem
sampleStations :: Map String (Set String)
sampleStations =
  Map.fromFoldable
    [ ("kone" /\ Set.fromFoldable ([ "id", "nv", "ut" ]))
    , ("ktwo" /\ Set.fromFoldable ([ "wa", "id", "mt" ]))
    , ("kthree" /\ Set.fromFoldable ([ "or", "nv", "ca" ]))
    , ("kfour" /\ Set.fromFoldable ([ "nv", "ut" ]))
    , ("kfive" /\ Set.fromFoldable ([ "ca", "az" ]))
    ]

sampleStates :: Set String
sampleStates = Set.fromFoldable [ "mt", "wa", "or", "id", "nv", "ut", "ca", "az" ]

getBestStation :: Map String (Set String) -> Set String -> Maybe { coveredStates :: Set String, station :: String }
getBestStation stations statesLeft =
  foldl
    ( \acc (station /\ coveredStates) -> case acc of
        Nothing -> Just { coveredStates: Set.intersection statesLeft coveredStates, station }
        Just prev ->
          let
            realCoveredStates = Set.intersection statesLeft coveredStates
          in
            if Set.size realCoveredStates > Set.size prev.coveredStates then
              Just { coveredStates: realCoveredStates, station }
            else
              Just prev
    )
    Nothing
    ((Map.toUnfoldable stations) :: Array (Tuple String (Set String)))

getStations :: Map String (Set String) -> Set String -> Maybe (Array String)
getStations stations statesToCover = go stations statesToCover []
  where
  go stationsLeft statesLeft selectedStations =
    if (Set.size statesLeft == 0) then
      Just $ selectedStations
    else do
      { coveredStates, station } <- getBestStation stationsLeft statesLeft
      let
        newStatesLeft = Set.difference statesLeft coveredStates

        newStationsLeft = Map.delete station stationsLeft
      go newStationsLeft newStatesLeft (station : selectedStations)

main :: Effect Unit
main = logShow $ getStations sampleStations sampleStates
