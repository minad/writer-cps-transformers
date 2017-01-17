{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif

module Control.Monad.Trans.RWS.CPS.Internal (
  RWST(..)
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
-- | A monad transformer adding reading an environment of type @r@,
-- collecting an output of type @w@ and updating a state of type @s@
-- to an inner monad @m@.
--
-- The constructor is only exported in the Internal module because it allows
-- access to the hidden state w.
newtype RWST r w s m a = RWST { unRWST :: r -> s -> w -> m (a, s, w) }

instance Functor m => Functor (RWST r w s m) where
  fmap f m = RWST $ \r s w -> (\(a, s', w') -> (f a, s', w')) <$> unRWST m r s w
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (RWST r w s m) where
  pure a = RWST $ \_ s w -> return (a, s, w)
  {-# INLINE pure #-}

  RWST mf <*> RWST mx = RWST $ \r s w -> do
    (f, s', w')  <- mf r s w
    (x, s'', w'') <- mx r s' w'
    return (f x, s'', w'')
  {-# INLINE (<*>) #-}

instance (Functor m, MonadPlus m) => Alternative (RWST r w s m) where
  empty = RWST $ \_ _ _ -> mzero
  {-# INLINE empty #-}

  RWST m <|> RWST n = RWST $ \r s w -> m r s w `mplus` n r s w
  {-# INLINE (<|>) #-}

instance Monad m => Monad (RWST r w s m) where
#if !(MIN_VERSION_base(4,8,0))
  return a = RWST $ \_ s w -> return (a, s, w)
  {-# INLINE return #-}
#endif

  m >>= k = RWST $ \r s w -> do
    (a, s', w')  <- unRWST m r s w
    unRWST (k a) r s' w'
  {-# INLINE (>>=) #-}

  fail msg = RWST $ \_ _ _ -> fail msg
  {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail m => Fail.MonadFail (RWST r w s m) where
  fail msg = RWST $ \_ _ _ -> Fail.fail msg
  {-# INLINE fail #-}
#endif

instance (Functor m, MonadPlus m) => MonadPlus (RWST r w s m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadFix m => MonadFix (RWST r w s m) where
  mfix f = RWST $ \r s w -> mfix $ \ ~(a, _, _) -> unRWST (f a) r s w
  {-# INLINE mfix #-}

instance MonadTrans (RWST r w s) where
  lift m = RWST $ \_ s w -> do
    a <- m
    return (a, s, w)
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (RWST r w s m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}
