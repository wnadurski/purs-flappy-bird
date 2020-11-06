module Enemy where

import Prelude
import Data.Array (range, zip)
import Data.Component (Component(..), mkCollider, mkTransform)
import Data.Entity (Entity(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3, tuple4, (/\))
import Purs.Data.ZipArray (ZipArray(..))
import Purs.Random (randomN, randomNR)
import Random.LCG (Seed)
import Resources (Resources)

enemiesDistance :: Int
enemiesDistance = 500

absoluteXShift :: Int
absoluteXShift = 100

meanBottomYPos :: Int
meanBottomYPos = 320

meanTopYPos :: Int
meanTopYPos = -200

absoluteYShift :: Int
absoluteYShift = 100

data Side
  = Top
  | Bottom

sideToFactor :: Side -> Number
sideToFactor Bottom = 1.0

sideToFactor Top = -1.0

mkEnemy :: Resources -> Int -> Number -> Number -> Side -> Entity
mkEnemy resources idCount xPos yPos side =
  Entity
    [ Id $ "octopus-" <> (show idCount)
    , mkTransform (Just { x: xPos, y: yPos }) (Just ({ x: 4.0, y: 4.0 * (sideToFactor side) }))
    , CanvasSpriteRenderer resources.octopus
    , mkCollider { x: 46.0, y: 480.0 } { x: 28.0, y: 18.0 }
    ]

initialEnemies :: Seed -> Resources -> Tuple (Array Entity) Seed
initialEnemies seed resources =
  let
    (xRandoms /\ newSeed) = randomNR 3 (-absoluteXShift) absoluteXShift seed

    (yRandoms /\ newSeed2) = randomNR 3 (-absoluteYShift) absoluteYShift newSeed

    (sideRandom /\ newSeed3) = randomNR 3 0 1 newSeed2

    enemies =
      (unwrap $ tuple4 <$> ZipArray (range 1 3) <*> ZipArray xRandoms <*> ZipArray yRandoms <*> ZipArray sideRandom)
        <#> ( \(index /\ randomX /\ randomY /\ randomSide /\ _) ->
              index /\ (toNumber $ randomX + (index * enemiesDistance)) /\ (toNumber $ randomY + (if randomSide == 1 then meanTopYPos else meanBottomYPos)) /\ (if randomSide == 1 then Top else Bottom)
          )
        <#> \(index /\ xPos /\ yPos /\ side) -> mkEnemy resources index xPos yPos side
  in
    enemies /\ newSeed3
