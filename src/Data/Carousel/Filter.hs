{-# Language BlockArguments #-}
module Data.Carousel.Filter
  ( Filter(..)
  ) where

import Data.Default
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
import Network.Socket

newtype Filter a = Filter
  { runFilter :: a -> SockAddr -> IO Bool
  } 

instance Contravariant Filter where
  contramap f (Filter p) = Filter (p . f)

instance Divisible Filter where
  conquer = Filter $ \_ _ -> pure True
  divide f (Filter p) (Filter q) = Filter $ \ a t -> case f a of
    (b, c) -> p b t >>= \br -> if br then q c t else pure False

instance Decidable Filter where
  choose f (Filter p) (Filter q) = Filter $ \a t -> case f a of
    Left b -> p b t
    Right c -> q c t
  lose f = Filter (absurd . f)

instance Default (Filter a) where
  def = conquer

instance Semigroup (Filter a) where
  (<>) = divide \a -> (a,a)

instance Monoid (Filter a) where
  mempty = conquer
