module Test.Purs.Data.ZipArray where

import Prelude

import Control.Monad.Free (Free)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3, (/\))
import Purs.Data.ZipArray (ZipArray(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

add3 :: forall a. Semiring a => a -> a -> a -> a
add3 a b c = a + b + c

tests :: Free TestF Unit
tests =
  suite "ZipArray" do
    test "adds two arrays" do
      Assert.equal (ZipArray [ 4, 6 ]) ((+) <$> (ZipArray [ 1, 2 ]) <*> (ZipArray [ 3, 4 ]))
    test "adds three arrays" do
      Assert.equal (ZipArray [ 5, 7 ]) (add3 <$> (ZipArray [ 1, 2 ]) <*> (ZipArray [ 3, 4 ]) <*> (ZipArray [ 1, 1 ]))
    test "creates tuple from 2 arrs" do
      Assert.equal (ZipArray [ 1 /\ 3, 2 /\ 4 ]) ((/\) <$> (ZipArray [ 1, 2 ]) <*> (ZipArray [ 3, 4 ]))
    test "creates tuple from 3 arrs" do
      Assert.equal (ZipArray [ tuple3 1 3 1, tuple3 2 4 1 ]) (tuple3 <$> (ZipArray [ 1, 2 ]) <*> (ZipArray [ 3, 4 ]) <*> (ZipArray [ 1, 1 ]))
