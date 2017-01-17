{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif

module Control.Monad.Trans.Writer.CPS.Internal (
  WriterT(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while '>>='
-- combines the outputs of the subcomputations using 'mappend'.
--
-- The constructor is only exported in the Internal module because it allows
-- access to the hidden state w.
newtype WriterT w m a = WriterT { unWriterT :: w -> m (a, w) }

instance Functor m => Functor (WriterT w m) where
  fmap f m = WriterT $ \w -> (\(a, w') -> (f a, w')) <$> unWriterT m w
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (WriterT w m) where
  pure a = WriterT $ \w -> return (a, w)
  {-# INLINE pure #-}

  WriterT mf <*> WriterT mx = WriterT $ \w -> do
    (f, w') <- mf w
    (x, w'') <- mx w'
    return (f x, w'')
  {-# INLINE (<*>) #-}

instance (Functor m, MonadPlus m) => Alternative (WriterT w m) where
  empty = WriterT $ const mzero
  {-# INLINE empty #-}

  WriterT m <|> WriterT n = WriterT $ \w -> m w `mplus` n w
  {-# INLINE (<|>) #-}

instance Monad m => Monad (WriterT w m) where
#if !(MIN_VERSION_base(4,8,0))
  return a = WriterT $ \w -> return (a, w)
  {-# INLINE return #-}
#endif

  m >>= k = WriterT $ \w -> do
    (a, w') <- unWriterT m w
    unWriterT (k a) w'
  {-# INLINE (>>=) #-}

  fail msg = WriterT $ \_ -> fail msg
  {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail m => Fail.MonadFail (WriterT w m) where
  fail msg = WriterT $ \_ -> Fail.fail msg
  {-# INLINE fail #-}
#endif

instance (Functor m, MonadPlus m) => MonadPlus (WriterT w m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadFix m => MonadFix (WriterT w m) where
  mfix f = WriterT $ \w -> mfix $ \ ~(a, _) -> unWriterT (f a) w
  {-# INLINE mfix #-}

instance MonadTrans (WriterT w) where
  lift m = WriterT $ \w -> do
    a <- m
    return (a, w)
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (WriterT w m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}
