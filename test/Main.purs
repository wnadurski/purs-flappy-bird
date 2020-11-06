module Test.Main where

import Prelude

import Effect (Effect)
import Test.Entity as Entity
import Test.Purs.Data.ZipArray as ZipArray
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Entity.tests
  ZipArray.tests
