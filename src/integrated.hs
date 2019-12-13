module Integrated where


import Control.Monad
import System.Random as Random
import Data.Traversable
import qualified Shrinking as QC

import Rose

newtype Gen a =
  Gen {
    runGen :: Random.StdGen -> Rose a
  }


instance Functor Gen where
  f `fmap` (Gen g) =
    Gen $ \seed ->
      fmap f (g seed)


instance Applicative Gen where
  pure =
    Gen . const . pure
  (<*>) =
    ap


instance Monad Gen where
  return = pure
  x >>= f =
    Gen $ \seed -> do
      let
        (seed0, seed1) = Random.split seed
      node  <- runGen x seed0
      runGen (f node) seed1


arbitrary :: QC.Arbitrary a => Gen a
arbitrary = Gen $ \s ->
  let a = QC.runGen (QC.arbitrary) s
   in unfold QC.shrink a


integral :: (Random i, Integral i) => i -> i -> Gen i
integral lower upper =
  Gen $ \seed ->
    pure . fst $
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
      Property . pure $ pure result


instance Testable Property where
  property = id


forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll gen prop =
  Property $ do
    a <- gen
    runProperty $
      mapResult (addInput (show a)) $
        (property (prop a))


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
