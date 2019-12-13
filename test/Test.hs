{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad.IO.Class
import           Foreign.C.Types (CSize, CInt, CUChar)

import           Hedgehog
import           Hedgehog.Main (defaultMain)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           CircularBuffer (CircularBuffer)
import qualified CircularBuffer as Circular


-- State


data State v =
    State {
      capacity :: CSize
    , buffer   :: Maybe (Var CircularBuffer v)
    , contents :: [CUChar]
    }


initialState :: State v
initialState = State 0 Nothing []


--- Create ---


data Create (v :: * -> *) =
  Create CSize
  deriving (Eq, Show)

instance HTraversable Create where
  htraverse _ (Create i) =
    pure (Create i)

create :: (MonadGen n, MonadIO m) => Command n m State
create =
  let
    gen (State _ Nothing _) =
      Just $ Create <$> Gen.integral (Range.linear 1 50)

    gen _ = Nothing

    execute (Create i) = do
      liftIO (Circular.create i)

   in
    Command gen execute [
        Update $ \_ (Create i) o ->
          State i (Just o) []
      ]


--- Put ---


data Put (v :: * -> *) =
  Put (Var CircularBuffer v) CUChar
  deriving (Eq, Show)

instance HTraversable Put where
  htraverse f (Put buf val) =
    Put
      <$> htraverse f buf
      <*> pure val

put :: (MonadGen n, MonadIO m) => Command n m State
put =
  let
    gen (State _ Nothing _) = Nothing
    gen (State _ (Just buf) _) =
      Just $
        Put buf <$> Gen.integral (Range.linear 0 100)

    execute (Put c i) = do
      liftIO (Circular.put (concrete c) i)

   in
    Command gen execute [
        Update $ \s (Put _ v) _ ->
          s {
            contents = take (fromIntegral (capacity s)) (v : contents s)
          }
      ]


--- Get ---


data Get (v :: * -> *) =
  Get (Var CircularBuffer v)
  deriving (Eq, Show)

instance HTraversable Get where
  htraverse f (Get buffer) =
    Get
      <$> htraverse f buffer

get :: (MonadGen n, MonadIO m) => Command n m State
get =
  let
    gen (State _ Nothing _) = Nothing
    gen (State _ (Just buffer) _) =
      Just $ pure (Get buffer)

    execute (Get c) = do
      liftIO (Circular.get (concrete c))

    safeLast = foldl (const Just) Nothing
    safeInit x = take (length x - 1) x

   in
    Command gen execute [
        Update $ \s _ _ ->
          s {
            contents = safeInit (contents s)
          }

      , Ensure $ \s _ _ o ->
          safeLast (contents s) === o
      ]


--- Tests ---


prop_registry_sequential :: Property
prop_registry_sequential =
  withTests 1000 . property $ do
    actions <- forAll $
      Gen.sequential
        (Range.linear 1 100)
        initialState
        [create, put, get]

    executeSequential initialState actions



prop_registry_parallel :: Property
prop_registry_parallel =
  withTests 1000 . withRetries 50 . property $ do
    actions <- forAll $
      Gen.parallel
        (Range.linear 1 10)
        (Range.linear 1 10)
        initialState
        [create, put, get]

    test $
      executeParallel initialState actions


main :: IO ()
main =
  defaultMain [
    checkSequential $$(discover)
  ]
