module Purs.Random (randomN, randomNR) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Random.LCG (Seed, lcgNext, unSeed)

randomN :: Int -> Seed -> Tuple (Array Int) Seed
randomN 0 seed = [] /\ seed

randomN n seed =
  let
    nextSeed = lcgNext seed

    (rest /\ latestSeed) = randomN (n - 1) nextSeed
  in
    ([ unSeed nextSeed ] <> rest) /\ latestSeed

randomNR :: Int -> Int -> Int -> Seed -> Tuple (Array Int) Seed
randomNR n from to seed = spy ("hello2 " <> show from <> " " <> show to) $ (map cut) `lmap` randomN n seed
  where
  difference = to - from

  cut r = (r `mod` (difference + 1)) + from
