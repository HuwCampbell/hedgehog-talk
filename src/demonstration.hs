module Demonstration where

import Control.Monad
import System.Random as Random
import Rose

newtype Gen a =
  Gen {
    runGen :: Random.StdGen -> Rose a
  }

instance Functor Gen where
  fmap f (Gen g) =
    Gen $ \s -> fmap f (g s)

instance Applicative Gen where
  pure = Gen . const . pure
  (<*>) = ap

instance Monad Gen where
  return = pure
  Gen g >>= f =
    Gen $ \seed -> do
      let (seed0, seed1) = Random.split seed
      node <- g seed0
      runGen (f node) seed1

class Arbitrary a where
  arbitrary :: Gen a

instance Arbitrary Bool where
  arbitrary =
    Gen $ \seed ->
      let shrink False = []
          shrink True = [False]
       in unfold shrink (fst $ Random.random seed)

data Result
  = Success
  | Failure String [String]
  deriving (Eq, Show)


failWith :: String -> Result
failWith r = Failure r []


addInput :: String -> Result -> Result
addInput _ Success = Success
addInput x (Failure r xx) = Failure r (x : xx)


newtype Property =
  Property {
    runProperty :: Gen (IO Result)
  }

mapResult :: (Result -> Result) -> Property -> Property
mapResult f prop =
  Property $ fmap (fmap f) $ runProperty prop

class Testable prop where
  property :: prop -> Property

instance Testable Property where
  property = id

instance Testable Bool where
  property b =
    let
      result = if b then Success else failWith "falsifiable"
    in
      Property . pure . pure $ result

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  property prop = forAll arbitrary prop

forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll gen prop =
  Property $ do
    a <- gen
    runProperty $
      mapResult (addInput (show a)) $
        property (prop a)


check :: Testable prop => prop -> IO Result
check prop = do
  let
    go 0 _ =
      return Success

    go i seed = do
      let
        (seed0, seed1) = Random.split seed
        rose = runGen (runProperty (property prop)) seed0

      result <- node rose
      if Success == result then go (i - 1) seed1 else findSmallest seed0 result (leaves rose)

  seed <- Random.newStdGen
  go 100 seed


findSmallest :: Random.StdGen -> Result -> [Rose (IO Result)] -> IO Result
findSmallest seed sofar potentials =
  case potentials of
    [] ->
      pure sofar
    (rose : roses) -> do
      result <- node rose
      if Success == result then
        findSmallest seed sofar roses
      else
        findSmallest seed result (leaves rose)

