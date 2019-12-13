module Rose where

import Control.Monad (ap)

data Rose a =
  Rose {
    node :: a
  , leaves :: [Rose a]
  } deriving (Eq, Show)

instance Functor Rose where
  fmap f ~(Rose x rs) =
    Rose (f x) (fmap (fmap f) rs)

instance Applicative Rose where
  pure x =
    Rose x []
  (<*>) =
    ap

instance Monad Rose where
  return =
    pure
  m >>= k =
    roseJoin (fmap k m)

instance Foldable Rose where
  foldMap f (Rose x ts) =
    f x <> foldMap (foldMap f) ts
  null _ = False

instance Traversable Rose where
  traverse f (Rose x ts) =
    Rose <$> (f x) <*> traverse (traverse f) ts

-- Specialized version of join
-- (to implement bind)
roseJoin :: Rose (Rose a) -> Rose a
roseJoin (Rose ~(Rose x ts) tts) =
  Rose x (map roseJoin tts ++ ts)

-- | Create a tree from a value and an unfolding function.
unfold :: (a -> [a]) -> a -> Rose a
unfold f x =
  Rose x (unfoldForest f x)

-- | Create a forest from a value and an unfolding function.
unfoldForest :: (a -> [a]) -> a -> [Rose a]
unfoldForest f =
  fmap (unfold f) . f
