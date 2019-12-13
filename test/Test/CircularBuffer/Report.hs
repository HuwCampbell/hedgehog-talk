{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.CircularBuffer.Report where

import           Control.Monad.IO.Class
import           Foreign.C.Types (CSize, CInt, CUChar)

import           Hedgehog (Group (..), checkSequential)
import           Hedgehog.Main (defaultMain)

import           StateMachine
import           Report

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

instance HFunctor Create where
  hmap _ (Create i) =
    Create i

instance HTraversable Create where
  htraverse _ (Create i) =
    pure (Create i)

create :: Command IO State
create =
  let
    gen (State _ Nothing _) =
      Just $ Create <$> integral 1 50

    gen _ = Nothing

    execute (Create i) = do
      liftIO (Circular.create i)

   in
    Command gen execute (
      \_ (Create i) o ->
        State i (Just o) []
    ) (\_ _ _ _ -> success)


--- Put ---


data Put (v :: * -> *) =
  Put (Var CircularBuffer v) CUChar

deriving instance Show (Var CircularBuffer v) => Show (Put v)

instance HFunctor Put where
  hmap f (Put buf val) =
    Put (hmap f buf) val

instance HTraversable Put where
  htraverse f (Put buf val) =
    Put
      <$> htraverse f buf
      <*> pure val

put ::  Command IO State
put =
  let
    gen (State _ Nothing _) = Nothing
    gen (State _ (Just buf) _) =
      Just $
        Put buf <$> integral 0 100

    execute (Put c i) = do
      liftIO (Circular.put (concrete c) i)

   in
    Command gen execute (
      \s (Put _ v) _ ->
        s {
          contents = take (fromIntegral (capacity s)) (v : contents s)
        }
    ) (\_ _ _ _ -> success)


--- Get ---


data Get (v :: * -> *) =
  Get (Var CircularBuffer v)

deriving instance Show (Var CircularBuffer v) => Show (Get v)


instance HFunctor Get where
  hmap f (Get buffer) =
    Get (hmap f buffer)

instance HTraversable Get where
  htraverse f (Get buffer) =
    Get
      <$> htraverse f buffer

get :: Command IO State
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
    Command gen execute (
      \s _ _ ->
        s {
          contents = safeInit (contents s)
        }
    ) (\s _ _ o -> property (safeLast (contents s) == o ))


--- Tests ---


prop_registry_sequential :: Property
prop_registry_sequential =
    forAll (generateActions initialState [create, put, get]) $
      executeActions initialState


tests :: IO Bool
tests =
  check prop_registry_sequential >>= \res ->
    printResult res >> return (res == Success)
