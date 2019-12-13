{-# Language KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}

module StateMachine where

import Report
import Rose

import Control.Monad (foldM)
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Data.Functor.Const
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map

import qualified System.Random as Random

import Unsafe.Coerce (unsafeCoerce)

import GHC.Types (Any)

{-

State machine testing is the generation of test programs,
including function inputs, outputs, and assertions...

Last week we saw a circular buffer example, and we were
modelling the creation of a buffer, adding items to it,
retrieving items from it, and ensuring that those items
were what we were expecting.

por exemplo:


> test :: TestT IO ()
> test = do
>   buf <- create 3
>
>   put buf 5
>
>   put buf 10
>
>   put buf 20
>
>   retrieved <- get buf
>   retrieved === Just 5


Which is quite different to:


> test :: TestT IO ()
> test = do
>   buf <- create 2
>
>   put buf 5
>
>   put buf 10
>
>   put buf 20
>
>   retrieved <- get buf
>   retrieved === Just 10


Obviously, in order to generate the correct assertion
at the end, we need to be keeping track of what we think
the should be holding; in other words, we need to model
the state of the program.


This sounds difficult, looking at the last action,
which includes a test:


To generate this step, we need to know:

    - If a buffer will exists;
    - The "name" of the variable of a buffer;
    - What's been placed into this particular buffer
      (which doesn't exist yet);
    - How to access the intermediate output from the
      get command itself.


To run this step, we need to know:

    - How to find the real buffer which we have called
      by name;
    - The true results from our get to compare in our
      assertion.


So we're going to need a way to wrap over not the actual
values of a type, but rather the way that they are held.


Who's heard of Higher Kinded data types?

A higher kinded data type is one which is parameterised
by a higher kind (usually a Functor).

-}

data HKPerson h =
  HKPerson {
    name :: h String
  , age  :: h Int
  }


type KnownPerson       = HKPerson Identity
type UnknownPerson     = HKPerson Maybe
type ExplainedPerson a = HKPerson (Either a)

toUnknown :: HKPerson Identity -> HKPerson Maybe
toUnknown (HKPerson (Identity n) (Identity a)) =
  HKPerson (Just n) (Just a)


{-

These can be really useful is parsing situations, or when
some values of the type are only available at a certain phase
of the program.


Inside the CIJ codebase for example, there are data types prefixed
with "raw", which are almost the same type, but with everything
wrapped in an Option.


Without a good abstraction though, it can be frustrating to move
between them, as one constantly has to wrap and rewrap the data
constructors.


Introducing: the HFunctor

-}


class HFunctor g where
  hmap :: (forall a. m a -> n a) -> g m -> g n


{-

As opposed to the more standard Functor and MFunctor

-}


class Functor f where
  map :: (a -> b) -> f a -> f b

class MFunctor g where
  hoist :: (forall a. m a -> n a) -> g m a -> g n a


{-

Which operate on standard Functor and Monad Transformers
respectively.

An implementation of HFunctor is usually trivial to write.

-}


instance HFunctor HKPerson where
  hmap f (HKPerson n a) = HKPerson (f n) (f a)


toUnknown' :: HFunctor h => h Identity -> h Maybe
toUnknown' = hmap (Just . runIdentity)


forget :: HFunctor h => h (Either a) -> h Maybe
forget = hmap eToM
  where
    eToM (Right a) = Just a
    eToM (Left _) = Nothing


{-

But some things are harder to move between, I can
go from an HKPerson Identity to an HKPerson Maybe person easily
by putting everything in a Just (or as const Nothing alternatively),
but I can't move from an HKPerson Maybe to a HKPerson Identity
unless all the fields are actually present.

So I can do it... maybe

-}


class HFunctorMaybe g where
  hmapMaybe :: (forall a. m a -> Maybe (n a)) -> g m -> Maybe (g n)

{-

But of course this can be generalised for any applicative

-}

class HTraversable g where
  htraverse :: forall m n e. Applicative e => (forall a. m a -> e (n a)) -> g m -> e (g n)


instance HTraversable HKPerson where
  htraverse f (HKPerson n a)=
    HKPerson
      <$> f n
      <*> f a



toKnown :: HTraversable h => h Maybe -> Maybe (h Identity)
toKnown = htraverse (fmap Identity)



--
-- A free higher kinded functor called Var is used to wrap
-- a single value of type 'a'
--
data Var a h = Var (h a)
  deriving (Eq, Show)


instance HFunctor (Var a) where
  hmap f (Var x) = Var (f x)


instance HTraversable (Var a) where
  htraverse f (Var x) = Var <$> f x


{-

So here's the plan:

In generation phase, every bind will be replace with
a unique name, which will be an incrementing integer.

> test :: TestT IO ()
> test = do
>   buf <- create 3
>
>   put buf 5
>
>   put buf 10
>
>   put buf 20
>
>   retrieved <- get buf
>   retrieved === Just 5

during generation becomes

> test :: TestT IO ()
> test = do
>   Var 1 = create 3
>
>   Var 2 = put (Var 1) 5
>
>   Var 3 = put (Var 1) 10
>
>   Var 4 = put (Var 1) 20
>
>   Var 5 = get (Var 1)
>   Var 5 === Just 5

And at execution time, we substitute out the
integer representations for actual values.

-}

type Name = Int

-- | data Const a b = Const a
data Symbolic a = Symbolic Name
  deriving Show

-- | data Identity a = Identity a
newtype Concrete a = Concrete a


concrete :: Var a Concrete -> a
concrete (Var (Concrete a)) = a


data Command m state =
  forall input output. (HFunctor input, Show (input Symbolic), Show (Symbolic output)) =>
  Command {
    -- | A generator which provides random arguments for a command. If the
    --   command cannot be executed in the current state, it should return
    --   'Nothing'.
      commandGen ::
        state Symbolic -> Maybe (Gen (input Symbolic))

    -- | Executes a command using the arguments generated by 'commandGen'.
    , commandExecute ::
        input Concrete -> m output

    -- | Update the state during generation or execution.
    , commandUpdate ::
        forall v. state v -> input v -> Var output v -> state v

    -- | What conditions must hold once the command has executed for real.
    , commandEnsure ::
        state Concrete -> state Concrete -> input Concrete -> output -> Property

    }


data Intermediate m state =
  forall input output. (HFunctor input, Show (input Symbolic), Show (Symbolic output)) =>
  Intermediate {
    -- | A generator which provides random arguments for a command.
      intermediateGen ::
        Gen (input Symbolic)

    -- | Executes a command using the arguments generated by 'commandGen'.
    , intermediateExecute ::
        input Concrete -> m output

    -- | Update the state during generation or execution.
    , intermediateUpdate ::
        forall v. state v -> input v -> Var output v -> state v

    -- | What conditions must hold once the command has executed for real.
    , intermediateEnsure ::
        state Concrete -> state Concrete -> input Concrete -> output -> Property

    }

-- | An instantiation of a 'Command' which can be executed, and its effect
--   evaluated.
--
data Action m state =
  forall input output. (HFunctor input, Show (input Symbolic), Show (Symbolic output)) =>
  Action {
      actionInput ::
        input Symbolic

    , actionOutput ::
        Symbolic output

    , actionExecute ::
        input Concrete -> m output

    , actionUpdate ::
        forall v. state v -> input v -> Var output v -> state v

    , actionEnsure ::
        state Concrete -> state Concrete -> input Concrete -> output -> Property
    }


instance Show (Action m state) where
  show (Action input output _ _ _) =
    show output ++ " <- " ++ show input

  showList actions s =
    unlines (fmap show actions) ++ s



-- | Generate a single action from a list of commands
generateAction :: Name -> state Symbolic -> [Command m state] -> Gen (Action m state)
generateAction name state commands = do
  let
    applicable =
      [ Intermediate gen execute update ensure
      | Command cGen execute update ensure <- commands
      , Just gen <- [cGen state]
      ]

  Intermediate gen execute update ensure
    <- element applicable

  input
    <- gen

  return $
    Action input (Symbolic name) execute update ensure


-- | Generate a random length set of actions
--
--   We're not doing any shrinking here.
generateActions :: state Symbolic -> [Command m state] -> Gen [Action m state]
generateActions initialState commands = do
  number       <- integral 1 10
  (actions, _) <- foldM go ([], initialState) [1 .. number]
  return (reverse actions)

  where
    go (rest, state) name = do
      action@(Action input output _ update _) <- generateAction name state commands
      let
        newState =
          update state input (Var output)

      return $
        (action : rest, newState)



type Environment = Map Name Any


reify :: Environment -> Symbolic a -> Concrete a
reify env (Symbolic n) =
  unsafeCoerce $
    env Map.! n


updateEnvironment :: Environment -> Symbolic a -> a -> Environment
updateEnvironment env (Symbolic n) concrete =
  Map.insert n (unsafeCoerce concrete) env


-- | Execute an action
--
--   We need to be inside a Gen as we are going to be running a property
--   test to ensure the state is good.
executeAction :: Action IO state -> Gen (StateT (state Concrete, Environment) IO Result)
executeAction (Action input output execute update ensure) =
  Gen $ \seed -> do
    (state, env) <- get

    let
      input' =
        hmap (reify env) input

    output' <- liftIO (execute input')

    let
      newEnv =
        updateEnvironment env output output'

      newState =
        update state input' (Var (Concrete output'))

    put (newState, newEnv)

    liftIO $ runGen (runProperty (ensure state newState input' output')) seed


-- Execute all the actions given an initial state.
--
-- Returning the full Property
executeActions :: state Concrete -> [Action IO state] -> Property
executeActions initialState actions =
  Property $ do
    Gen $ \seed -> do
      (res, _) <-
        flip runStateT (initialState, Map.empty) $
          goActions seed Success actions
      return res

  where
    goActions _    result@(Failure {}) _ = return result
    goActions _    result [] = return result
    goActions seed Success (a : as) = do
      let
        (seed0, seed1) = Random.split seed
      result <- runGen (executeAction a) seed
      goActions seed1 result as
