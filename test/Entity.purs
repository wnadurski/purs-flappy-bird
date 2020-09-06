module Test.Entity where

import Prelude
import Color.Scheme.MaterialDesign (amber)
import Control.Monad.Free (Free)
import Data.Component (Component(..), isCamera, isDrawingRenderer, isPlayer, mkTransform)
import Data.Entity (Entity(..), hasComponent, hasComponents)
import Data.Maybe (Maybe(..))
import Data.Predicate (Predicate(..))
import Graphics.Drawing (fillColor, filled, rectangle)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

entity :: Entity
entity = Entity [ Camera, DrawingRenderer (filled (fillColor amber) (rectangle 0.0 1.0 2.0 3.0)), mkTransform (Just { x: 10.0, y: 20.0 }) Nothing ]

ask :: forall a. Predicate a -> a -> Boolean
ask (Predicate p) = p

tests :: Free TestF Unit
tests =
  suite "Entity's queries" do
    test "hasComponent should work" do
      Assert.equal (ask (hasComponent isCamera) entity) true
      Assert.equal (ask (hasComponent isPlayer) entity) false
      Assert.equal (ask (hasComponent isCamera && hasComponent isDrawingRenderer) entity) true
      Assert.equal (ask (hasComponent isPlayer || hasComponent isDrawingRenderer) entity) true
    test "hasComponents for one existing" do
      Assert.equal (ask (hasComponents [ isCamera ]) entity) true
    test "hasComponents for one not existing" do
      Assert.equal (ask (hasComponents [ isPlayer ]) entity) false
    test "hasComponents for two existing" do
      Assert.equal (ask (hasComponents [ isCamera, isDrawingRenderer ]) entity) true
    test "hasComponents for two not existing" do
      Assert.equal (ask (hasComponents [ isPlayer, isDrawingRenderer ]) entity) false
