module GrokkingAlgos.HashTable where

import Prelude
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

-- | Grocery prices
prices :: Map String Number
prices = fromFoldable [ Tuple "Apple" 0.67, Tuple "Milk" 1.46, Tuple "Avocado" 1.55 ]

-- | New voters
logVote :: String -> Map String Boolean -> Effect (Map String Boolean)
logVote name votersMap = ado
  logShow msg
  in newMap
  where
  Tuple msg newMap = checkVoter name votersMap

checkVoter :: String -> Map String Boolean -> Tuple String (Map String Boolean)
checkVoter name votersMap = case (lookup name votersMap) of
  Just _ -> Tuple "kick them out!" votersMap
  Nothing -> Tuple "let them vote!" (insert name true votersMap)

main :: Effect Unit
main = do
  logShow $ lookup "Apple" prices
  let
    empty = fromFoldable []
  y <- logVote "John" empty
  _ <- logVote "John" y
  pure unit
