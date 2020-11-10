module Purs.Random (randomN, randomNR, randomRange) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
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
randomNR n from to seed = (map cut) `lmap` randomN n seed
  where
  difference = to - from

  cut r = (r `mod` (difference + 1)) + from

randomRange :: Int -> Int -> Seed -> Tuple Int Seed
randomRange from to seed = (cut $ unSeed nextSeed) /\ nextSeed
  where
  nextSeed = lcgNext seed

  difference = to - from

  cut r = (r `mod` (difference + 1)) + from
