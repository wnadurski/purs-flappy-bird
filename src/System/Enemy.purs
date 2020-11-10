module Enemy where

import Prelude

import Control.Bind (bindFlipped)
import Control.Semigroupoid (composeFlipped)
import Data.Array (foldl, range, zip)
import Data.Component (Component(..), getId, getTransformData, isCamera, isId, isIdEqual, isTransform, mkCollider, mkTransform)
import Data.Entity (Entity(..), EntityFilter, eqId, getComponent, getEntityId, hasComponent, mapComponentsWith)
import Data.Int (toNumber)
import Data.Int.Bits (xor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Predicate (Predicate(..))
import Data.Scene (Scene, filterEntities, getEntity, mapEntities)
import Data.String.Utils (startsWith)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, Tuple4, tuple3, tuple4, (/\))
import Data.Vector2 (Point, Vector2)
import FRP.Behavior.Mouse (position)
import Purs.Data.ZipArray (ZipArray(..))
import Purs.Random (randomN, randomNR, randomRange)
import Random.LCG (Seed, mkSeed, unSeed)
import Record (get)
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

colliderSize :: Vector2
colliderSize = { x: 46.0, y: 480.0 }

mkEnemyTransform :: Number -> Number -> Side -> Component
mkEnemyTransform xPos yPos side = mkTransform (Just { x: xPos, y: yPos }) (Just ({ x: 4.0, y: 4.0 * (sideToFactor side) }))

mkEnemy :: Resources -> Int -> Number -> Number -> Side -> Entity
mkEnemy resources idCount xPos yPos side =
  Entity
    [ Id $ "octopus-" <> (show idCount)
    , mkEnemyTransform xPos yPos side
    , CanvasSpriteRenderer resources.octopus
    , mkCollider colliderSize { x: 28.0, y: 18.0 }
    ]

mkEnemyParameters :: Seed -> Int -> {seed :: Seed, params :: Tuple4 Int Number Number Side}
mkEnemyParameters seed index  =
  let
    (randomX /\ newSeed) = randomRange (-absoluteXShift) absoluteXShift $ seed

    (randomY /\ newSeed2) = randomRange (-absoluteYShift) absoluteYShift newSeed

    (randomSide /\ newSeed3) = randomRange 0 1 newSeed2
  in 
    { seed: newSeed3
    , params: index /\ (toNumber $ randomX + (index * enemiesDistance)) /\ (toNumber $ randomY + (if randomSide == 1 then meanTopYPos else meanBottomYPos)) /\ (if randomSide == 1 then Top else Bottom) /\ unit
    }
  

initialEnemies :: Seed -> Resources -> Tuple (Array Entity) Seed
initialEnemies seed resources =
  let
    allParams = (range 1 3) 
      # foldl (\acc index -> 
        let 
          newResults = mkEnemyParameters acc.seed index 
        in 
          {seed: newResults.seed, params: acc.params <> [newResults.params]}
        ) {seed, params: []}
    enemies = 
      allParams.params
        <#> \(index /\ xPos /\ yPos /\ side /\ unit) -> mkEnemy resources index xPos yPos side
  in
    enemies /\ allParams.seed

isEnemy :: EntityFilter
isEnemy =
  Predicate
    $ getEntityId
    >>> map (startsWith "octopus")
    >>> fromMaybe false

hasPositionLike :: (Point -> Boolean) -> EntityFilter
hasPositionLike f =
  Predicate
    $ getComponent isTransform
    >>> bindFlipped getTransformData
    >>> map (get (SProxy :: SProxy "position"))
    >>> map f
    >>> fromMaybe false

enemiesSystem :: Seconds -> Scene -> Scene
enemiesSystem delta scene = fromMaybe scene mbNewScene
  where
  mbNewScene = do
    cameraPos <-
      getEntity scene (hasComponent isCamera)
        >>= getComponent isTransform
        >>= getTransformData
        <#> get (SProxy :: SProxy "position")

    entitiesIds <-
      (filterEntities scene $ isEnemy && hasPositionLike (\pos -> pos.x + (colliderSize.x * 1.5) < cameraPos.x))
        <#> getEntityId
        # sequence

    let 
      changeSceneForId id scene = 
        mapEntities (hasComponent $ isIdEqual id) (mapComponentsWith getTransformData $ \_ -> mkEnemyTransform xPos yPos side) scene
          # \scene -> scene { randomSeed = unSeed randomParams.seed, enemyIndex = index + 1}
        where
          randomParams = mkEnemyParameters (mkSeed scene.randomSeed) scene.enemyIndex
          index /\ xPos /\ yPos /\ side /\ unit = randomParams.params

    pure $ foldl composeFlipped identity (changeSceneForId <$> entitiesIds) scene
