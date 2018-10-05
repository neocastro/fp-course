{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose (Compose(..)) where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) :: (a -> b) -> Compose f g  a -> Compose f g b
  (<$>) f (Compose fga) = Compose ((f <$>) <$> fga)   


instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure  -- or Compose (pure (pure a)) 
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) (Compose fgab) (Compose fga) = Compose ((<*>) <$> fgab <*> fga) 

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
