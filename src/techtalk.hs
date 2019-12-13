module TechTalk where

import Control.Monad
import qualified System.Random as Random

import Rose

newtype Gen a =
  Gen {
    runGen :: Random.StdGen -> Rose a
  }

instance Functor Gen where
  fmap f (Gen g) =
    Gen $ \seed ->
      fmap f (g seed)

instance Applicative Gen where
  pure = Gen . const . pure
  (<*>) = ap

instance Monad Gen where
  return = pure
  x >>= f =
    Gen $ \seed -> do
      let
        (seed0, seed1) = Random.split seed
      node <- runGen x seed0
      runGen (f node) seed1

class Arbitrary a where
  arbitrary :: Gen a

instance Arbitrary Bool where
  arbitrary =
    let
      shrink b =
        if b then [False] else []
    in
      Gen $ \seed -> unfold shrink (fst $ Random.random seed)

data Result
  = Success
  | Failure String [String]
  deriving (Eq, Show)

addInput :: String -> Result -> Result
addInput _ Success = Success
addInput x (Failure r xs) =
  Failure r (x : xs)

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
      result = if b then Success else Failure "falsifiable" []
    in
      Property . pure  . pure $ result

instance (Show a, Arbitrary a, Testable prop) => Testable (a -> prop) where
  property f =
    forAll arbitrary f

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
      if Success == result then go (i - 1) seed1 else findSmallest result (leaves rose)

  seed <- Random.newStdGen
  go 100 seed

findSmallest :: Result -> [Rose (IO Result)] -> IO Result
findSmallest sofar potentials =
  case potentials of
    [] ->
      pure sofar
    (rose : roses) -> do
      result <- node rose
      if Success == result then
        findSmallest sofar roses
      else
        findSmallest result (leaves rose)
