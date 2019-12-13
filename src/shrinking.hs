module Shrinking where

import Control.Monad
import qualified System.Random as Random

import Rose

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
  shrink :: a -> [a]


instance Arbitrary Bool where
  arbitrary =
    Gen (fst . Random.random)
  shrink b =
    if b then [False] else []


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
    runProperty :: Gen (Rose (IO Result))
  }

mapResult :: (Result -> Result) -> Property -> Property
mapResult f prop =
  Property $ fmap (fmap (fmap f)) $ runProperty prop

class Testable prop where
  property :: prop -> Property

instance Testable Bool where
  property b =
    let
      result = if b then Success else failWith "falsifiable"
    in
      Property . pure . pure $ pure result

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  property f = forAllShrink arbitrary shrink f

instance Testable Property where
  property = id


forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll gen prop = forAllShrink gen (const []) prop


forAllShrink :: (Show a, Testable prop) => Gen a -> (a -> [a]) -> (a -> prop) -> Property
forAllShrink gen shr prop =
  Property $ do
    a <- gen
    runProperty $
      shrinking shr a $ \a' ->
        mapResult (addInput (show a')) $
          property (prop a')


shrinking :: (a -> [a]) -> a -> (a -> Property) -> Property
shrinking shrink x pf =
  Property $
    fmap join $
      traverse (runProperty . pf) $
        unfold shrink x


check :: Testable prop => prop -> IO Result
check prop = do
  let
    go 0 _ =
      return Success

    go i seed = do
      let
        (seed0, seed1) =
          Random.split seed

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
