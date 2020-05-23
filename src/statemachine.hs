{-# Language KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
{-# Language ExistentialQuantification #-}

module StateMachine where

import           ReportTeeny

import           Control.Monad (foldM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as Map

import qualified System.Random as Random

import           Unsafe.Coerce (unsafeCoerce)
import           GHC.Types (Any)

{-

Last week we talked a lot about Hedgehog and Quickcheck,
implementing small versions of both of them, and taking
a hard look at how they work internally.

Today we're going to talk about State Machine Testing,
which we only really got a small peek at.

So if I were to try and contrast the difference between
normally property based testing, it would be this:

> Usual property based testing is the random generation
  of inputs to functions under test;

> State machine testing is the random generation of
  programs with sensible assertions to test.


So state machine testing is the generation of test programs,
including function inputs, outputs, and all of the assertions
you would want to make as you were running it.


Last week we saw a circular buffer example, and we were
modelling the creation of a buffer, adding items to it,
retrieving items from it, and ensuring that those items
were what we were expecting.


If one was a java developer, they would almost certainly
test this by hand, and they might do something like this:


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
the state of the program. The programmer in this case
has used their mental model, but we want to do this
more rigorously.


This sounds difficult, just looking at the last action,
which includes a get action and a test assertion:


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


Inside the mneme codebase for example, there are data types prefixed
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

Compare to two other things named Functor: the standard Functor and the MFunctor

> class Functor f where
>   map :: (a -> b) -> f a -> f b
>
> class MFunctor g where
>   hoist :: (forall a. m a -> n a) -> g m a -> g n a


Which operate on singly kinded data types and monad transformers
respectively.

An implementation of HFunctor is usually trivial to write.

-}


instance HFunctor HKPerson where
  hmap f (HKPerson n a) = HKPerson (f n) (f a)


toUnknown' :: HFunctor h => h Identity -> h Maybe
toUnknown' = hmap (Just . runIdentity)


toEmptyUnknown :: HFunctor h => h x -> h Maybe
toEmptyUnknown = hmap (const Nothing)


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

This can be generalised for any applicative

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

-- | newtype Const a b = Const a
newtype Symbolic a = Symbolic Name
  deriving Show

-- | newtype Identity a = Identity a
newtype Concrete a = Concrete a


concrete :: Var a Concrete -> a
concrete (Var (Concrete a)) = a


{-

Our main Command type, which we will parameterise by the user's defined state type,
which needs to be one of these higher kinded data types.

The input and output are existentially quantified, so they don't appear in the
parameterised types of Command, and we can use any types with the Command itself.

This input type must be an HFunctor (or HTraversable in the case of Hedgehog proper).

-}

data Command state =
  forall input output. (HFunctor input, Show (input Symbolic)) =>
  Command {
    -- | A generator which provides random arguments for a command. If the
    --   command cannot be executed in the current state, it should return
    --   'Nothing'.
      commandGen ::
        state Symbolic -> Maybe (Gen (input Symbolic))

    -- | Executes a command using the arguments generated by 'commandGen'.
    , commandExecute ::
        input Concrete -> IO output

    -- | Update the state during generation or execution.
    , commandUpdate ::
        forall v. state v -> input v -> Var output v -> state v

    -- | What conditions must hold once the command has executed for real.
    , commandEnsure ::
        state Concrete -> state Concrete -> input Concrete -> output -> IO Result

    }


-- | Type mirroring Command but with concrete generator.
--
--   Only used during Action generation, and only required
--   due to existentially quantified types.
data Intermediate state =
  forall input output. (HFunctor input, Show (input Symbolic)) =>
  Intermediate {
    -- | A generator which provides random arguments for an action.
      intermediateGen ::
        Gen (input Symbolic)

    -- | Executes a command using the arguments generated by 'commandGen'.
    , intermediateExecute ::
        input Concrete -> IO output

    -- | Update the state during generation or execution.
    , intermediateUpdate ::
        forall v. state v -> input v -> Var output v -> state v

    -- | What conditions must hold once the command has executed for real.
    , intermediateEnsure ::
        state Concrete -> state Concrete -> input Concrete -> output -> IO Result

    }

-- | An instantiation of a 'Command' which can be executed, and its effect
--   evaluated.
--
data Action state =
  forall input output. (HFunctor input, Show (input Symbolic)) =>
  Action {
      actionInput ::
        input Symbolic

    , actionOutput ::
        Symbolic output

    , actionExecute ::
        input Concrete -> IO output

    , actionUpdate ::
        forall v. state v -> input v -> Var output v -> state v

    , actionEnsure ::
        state Concrete -> state Concrete -> input Concrete -> output -> IO Result
    }


instance Show (Action state) where
  show (Action input (Symbolic output) _ _ _) =
    "Var " ++ show output ++ " <- " ++ show input

  showList actions s =
    unlines (fmap show actions) ++ s



-- | Generate a single action from a list of commands
--
--   We need to pick which commands are applicable from the current state,
--   choose one, run its generator, and instantiate the Action.
--
generateAction :: Name -> state Symbolic -> [Command state] -> Gen (Action state)
generateAction name state commands = do
  let
    applicable =
      [ Intermediate gen execute update ensure
      | Command xGen execute update ensure <- commands
      , Just gen <- [xGen state]
      ]

  Intermediate gen execute update ensure
    <- element applicable

  input
    <- gen

  return $
    Action input (Symbolic name) execute update ensure


-- | Generate a random length set of actions
--
--   We need to keep track of and update the user defined state as
--   we go.
generateActions :: state Symbolic -> [Command state] -> Gen [Action state]
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



{-

We need a way to execute our actions, remember the constructor for Action contains
the following

>      actionInput ::
>        input Symbolic
>
>      actionExecute ::
>        input Concrete -> m output

so to execute, we somehow need to turn an `input Symbolic` into an `input Concrete`.

But where can we draw the concrete values from?

-}

newtype Environment = Environment {
  openEnvironment :: Map Name Any
}


newEnvironment :: Environment
newEnvironment =
  Environment Map.empty


{-

Hedgehog proper uses GHC's Data.Dynamic type, which adds some type safety. We're just
going to go yolo it.


Reifying a Symbolic to a Concrete means looking it up in the map, and coercing it to
the appropriate type.


-}


reify :: Environment -> Symbolic a -> Concrete a
reify (Environment env) (Symbolic n) =
  unsafeCoerce $
    env Map.! n


{-

We need a function to add more items into the environment.

-}

updateEnvironment :: Environment -> Symbolic a -> a -> Environment
updateEnvironment (Environment env) (Symbolic n) concrete =
  Environment $
    Map.insert n (unsafeCoerce concrete) env

{-

You might notice that I've put in 'a' while drawing out 'Concrete a';
fortunately as I've made sure Concrete is a newtype their runtime
representations are the same.

-}



{-

Execute a single action and run its tests


>  actionEnsure ::
>    state Concrete -> state Concrete -> input Concrete -> output -> IO Result


We'll also need access to our environment, and the state Concrete. So we're
going to use a StateT monad transformer over IO to obtain and update these states.


-}
executeAction :: Action state -> StateT (state Concrete, Environment) IO Result
executeAction (Action input output execute update ensure) = do
  (state, env) <- get

  let
    input' =
      hmap (reify env) input

  output' <- lift $ execute input'

  let
    newEnv =
      updateEnvironment env output output'

    newState =
      update state input' (Var (Concrete output'))

  put (newState, newEnv)
  lift (ensure state newState input' output')


{-

Execute all the actions given an initial state.

This will be out final property, so it's one of these

> Property = Property { runProperty :: Gen IO Result }

The idea is to run each action in turn, stopping if we
encounter a test failure.

-}
executeActions :: state Concrete -> [Action state] -> Property
executeActions initialState actions =
  Property $ do
    Gen $ \_ -> do
      evalStateT
        (goActions Success actions)
        (initialState, newEnvironment)

  where
    goActions Success (a : as) = do
      result <- executeAction a
      goActions result as

    goActions result _ = return result
