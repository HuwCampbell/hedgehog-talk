module Report where

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


integral :: (Random.Random i, Integral i) => i -> i -> Gen i
integral lower upper =
  Gen $ \seed ->
    fst $
      Random.randomR (lower, upper) seed


element :: [a] -> Gen a
element as = case as of
  [] ->
    error "Gen.element: used with empty list"
  _ -> do
    n <- integral 0 (length as - 1)
    pure $ as !! n


data Result
  = Success
  | Failure String [String]
  deriving (Eq, Show)


failWith :: String -> Result
failWith r = Failure r []


addInput :: String -> Result -> Result
addInput input (Failure r inputs) =
  Failure r (input : inputs)
addInput input Success = Success


newtype Property =
  Property {
    runProperty :: Gen (IO Result)
  }

mapResult :: (Result -> Result) -> Property -> Property
mapResult f prop =
  Property $ fmap (fmap f) $ runProperty prop

class Testable prop where
  property :: prop -> Property

instance Testable Bool where
  property b =
    let
      result = if b then Success else failWith "falsifiable"
    in
      Property . pure . pure $ result

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  property f =
    forAll arbitrary f


forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll gen f =
  Property $ do
    a <- gen
    runProperty $
      mapResult (addInput (show a)) $
        property (f a)


check :: Testable prop => prop -> IO Result
check prop = do
  let
    go 0 _ =
      return Success

    go i seed = do
      let
        (seed0, seed1) =
          Random.split seed

      result <- runGen (runProperty (property prop)) seed0
      if Success == result then go (i - 1) seed1 else return result

  seed <- Random.newStdGen
  go 100 seed
