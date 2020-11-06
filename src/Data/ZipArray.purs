module Purs.Data.ZipArray where

import Prelude

import Data.Array (zip)
import Data.Newtype (class Newtype)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))

newtype ZipArray a
  = ZipArray (Array a)

derive instance eqZipArray :: Eq a => Eq (ZipArray a)
derive newtype instance showZipArray :: Show a => Show (ZipArray a)
derive newtype instance functorZipArray :: Functor ZipArray
  
instance applyZipArray :: Apply ZipArray where
  apply :: forall a b. ZipArray (a -> b) -> ZipArray a -> ZipArray b
  apply (ZipArray fs) (ZipArray xs) = ZipArray (newArray $ fs /\ xs)
    where
      newArray = (uncurry zip) >>> map (uncurry ($))


instance newtypeZipArray :: Newtype (ZipArray a) (Array a) where
  wrap = ZipArray
  unwrap (ZipArray a) = a