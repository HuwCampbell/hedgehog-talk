module Real where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad.Trans.Resource
import Control.Monad.Morph

thisTest :: Property
thisTest = property $ do
  x <- forAll Gen.bool
  assert $ x || x


resourceFul :: Property
resourceFul = property $ do
  x <- forAll Gen.bool
  test . hoist runResourceT $ do
    annotate "yay"
    assert $ x || x
