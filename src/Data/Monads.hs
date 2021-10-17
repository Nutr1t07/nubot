{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Monads where

import Data.Bifunctor ( Bifunctor(first) )

-- 
-- class (forall m. Monad m => Monad (t m)) => MonadTrans t where
--   lift :: Monad m => m a -> t m a
-- 
-- 
-- newtype StateT s m a = StateT {
--   runStateT :: s -> m (a, s)
-- }
-- instance (Functor m) => Functor (StateT s m) where
--   fmap f (StateT x) = StateT (fmap (first f) . x)
-- 
-- instance (Applicative m) => Applicative (StateT s m) where
--   pure x = StateT (\s -> pure (x,s))
-- -- (StateT f) <*> (StateT x) = StateT (\s -> (f s) )
-- 
-- 
-- 
-- -- ## ReaderT
-- 
-- ask :: Monad m => ReaderT r m r
-- ask = ReaderT pure
-- 
-- asks :: Monad m => (r -> a) -> ReaderT r m a
-- asks f = ReaderT (pure.f)
-- 
-- newtype ReaderT r m a = ReaderT {
--   runReaderT :: r -> m a
-- }
-- 
-- instance MonadTrans (ReaderT r) where
--   lift = ReaderT . pure
-- 
-- instance (Functor m) => Functor (ReaderT r m) where
--   fmap f (ReaderT x) = ReaderT (fmap f.x)
-- 
-- instance (Applicative m) => Applicative (ReaderT r m) where
--   pure x = ReaderT $ pure (pure x)
--   (ReaderT f) <*> (ReaderT x) = ReaderT (\r -> f r <*> x r)
-- 
-- instance (Monad m) => Monad (ReaderT r m) where
--   (ReaderT x) >>= f = ReaderT (\r -> x r >>= (`runReaderT` r) . f)
-- 
