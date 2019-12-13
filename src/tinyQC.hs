module Tiny where

import Control.Monad
import qualified System.Random as Random


newtype Gen a =
  Gen {
    runGen :: Random.StdGen -> a
  }

instance Functor Gen where
  fmap f (Gen g) =
    Gen $ \seed ->
      f (g seed)

instance Applicative Gen where
  pure =
    Gen . const
  (<*>) =
    ap

instance Monad Gen where
  return = pure
  x >>= f =
    Gen $ \seed ->
      let (seed0, seed1) = Random.split seed
       in runGen (f (runGen x seed0)) seed1

class Arbitrary a where
  arbitrary :: Gen a

instance Arbitrary Bool where
  arbitrary =
    Gen (fst . Random.random)

newtype Property =
  Property {
    runProperty :: Gen Bool
  }

class Testable prop where
  property :: prop -> Property

instance Testable Bool where
  property = Property . pure

instance (Arbitrary a, Testable prop) => Testable (a -> prop) where
  property f =
    forAll arbitrary f

forAll :: Testable prop => Gen a -> (a -> prop) -> Property
forAll gen f =
  Property $
    gen >>=
      runProperty . property . f

check :: Testable prop => prop -> IO Bool
check prop = do
  let
    go 0 _ =
      return True

    go i seed = do
      let
        (seed0, seed1) =
          Random.split seed

        result = runGen (runProperty (property prop)) seed0
      if result then go (i - 1) seed1 else return result

  seed <- Random.newStdGen
  go 100 seed
