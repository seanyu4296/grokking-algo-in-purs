module GrokkingAlgos.Recursion where

import Prelude
import Control.Alt ((<|>))
import Data.Array (foldl)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Class.Console (logShow)

sumAll :: Array Int -> Int
sumAll arr = case Array.uncons arr of
  Nothing -> 0
  Just { head, tail } -> head + sumAll tail

lengthArr :: Array Int -> Int
lengthArr arr = case Array.uncons arr of
  Nothing -> 0
  Just { head, tail } -> 1 + lengthArr tail

max' :: Array Int -> Maybe Int
max' arr = do
  { head, tail } <- Array.uncons arr
  (max tail <#> \i -> if head > i then head else i) <|> (pure $ head)

max :: Array Int -> Maybe Int
max arr = case Array.uncons arr of
  Just { head, tail } ->
    let
      x = max tail
    in
      case x of
        Just i -> Just $ if head > i then head else i
        Nothing -> Just $ head
  Nothing -> Nothing

main :: Effect Unit
main = do
  logShow $ sumAll [ 1, 2, 3, 4, 5 ]
  logShow $ lengthArr [ 1, 2, 3, 4, 5 ]
  logShow $ max [ 1, 2, 3, 4, 5 ]
  logShow $ max' [ 1, 2, 3, 4, 5 ]
