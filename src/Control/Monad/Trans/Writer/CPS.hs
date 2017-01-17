{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Writer.CPS
-- Copyright   :  (c) Daniel Mendler 2016,
--                (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- The strict 'WriterT' monad transformer, which adds collection of
-- outputs (such as a count or string output) to a given monad.
--
-- This monad transformer provides only limited access to the output
-- during the computation. For more general access, use
-- "Control.Monad.Trans.State" instead.
--
-- This version builds its output strictly and uses continuation-passing-style
-- to achieve constant space usage. This transformer can be used as a
-- drop-in replacement for "Control.Monad.Trans.Writer.Strict".
-----------------------------------------------------------------------------

module Control.Monad.Trans.Writer.CPS (
  -- * The Writer monad
  Writer,
  writer,
  runWriter,
  execWriter,
  mapWriter,
  -- * The WriterT monad transformer
  -- The constructor is deliberately not exported to avoid exposing the
  -- hidden state of the Writer.
  WriterT,
  writerT,
  runWriterT,
  execWriterT,
  mapWriterT,
  -- * Writer operations
  tell,
  listen,
  listens,
  pass,
  censor,
  -- * Lifting other operations
  liftCallCC,
  liftCallCC',
  liftCatch,
) where

import Control.Monad.Trans.Writer.CPS.Internal (WriterT(..))
import Control.Monad.Signatures
import Data.Functor.Identity

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by the type @w@ of output to accumulate.
--
-- The 'return' function produces the output 'mempty', while '>>='
-- combines the outputs of the subcomputations using 'mappend'.
type Writer w = WriterT w Identity

-- | Construct a writer computation from a (result, output) pair.
-- (The inverse of 'runWriter'.)
writer :: (Monoid w, Monad m) => (a, w) -> WriterT w m a
writer (a, w') = WriterT $ \w -> let wt = w `mappend` w' in wt `seq` return (a, wt)
{-# INLINE writer #-}

-- | Unwrap a writer computation as a (result, output) pair.
-- (The inverse of 'writer'.)
runWriter :: Monoid w => Writer w a -> (a, w)
runWriter = runIdentity . runWriterT
{-# INLINE runWriter #-}

-- | Extract the output from a writer computation.
--
-- * @'execWriter' m = 'snd' ('runWriter' m)@
execWriter :: Monoid w => Writer w a -> w
execWriter = runIdentity . execWriterT
{-# INLINE execWriter #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriter' ('mapWriter' f m) = f ('runWriter' m)@
mapWriter :: (Monoid w, Monoid w') => ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = mapWriterT (Identity . f . runIdentity)
{-# INLINE mapWriter #-}

-- | The WriterT constructor is deliberately not exported to avoid exposing the
-- hidden state w.
-- This provides the safe way to construct a WriterT with the same api as the
-- original WriterT.
writerT :: (Functor m, Monoid w) => m (a, w) -> WriterT w m a
writerT f = WriterT $ \w -> (\(a, w') -> let wt = w `mappend` w' in wt `seq` (a, wt)) <$> f
{-# INLINE writerT #-}

-- | Unwrap a writer computation.
runWriterT :: Monoid w => WriterT w m a -> m (a, w)
runWriterT m = unWriterT m mempty
{-# INLINE runWriterT #-}

-- | Extract the output from a writer computation.
--
-- * @'execWriterT' m = 'liftM' 'snd' ('runWriterT' m)@
execWriterT :: (Monad m, Monoid w) => WriterT w m a -> m w
execWriterT m = do
  (_, w) <- runWriterT m
  return w
{-# INLINE execWriterT #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriterT' ('mapWriterT' f m) = f ('runWriterT' m)@
mapWriterT :: (Monad n, Monoid w, Monoid w') =>
  (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ \w -> do
  (a, w') <- f (runWriterT m)
  let wt = w `mappend` w'
  wt `seq` return (a, wt)
{-# INLINE mapWriterT #-}

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monoid w, Monad m) => w -> WriterT w m ()
tell w = writer ((), w)
{-# INLINE tell #-}

-- | @'listen' m@ is an action that executes the action @m@ and adds its
-- output to the value of the computation.
--
-- * @'runWriterT' ('listen' m) = 'liftM' (\\ (a, w) -> ((a, w), w)) ('runWriterT' m)@
listen :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m (a, w)
listen = listens id
{-# INLINE listen #-}

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
--
-- * @'runWriterT' ('listens' f m) = 'liftM' (\\ (a, w) -> ((a, f w), w)) ('runWriterT' m)@
listens :: (Monoid w, Monad m) => (w -> b) -> WriterT w m a -> WriterT w m (a, b)
listens f m = WriterT $ \w -> do
  (a, w') <- runWriterT m
  let wt = w `mappend` w'
  wt `seq` return ((a, f w'), wt)
{-# INLINE listens #-}

-- | @'pass' m@ is an action that executes the action @m@, which returns
-- a value and a function, and returns the value, applying the function
-- to the output.
--
-- * @'runWriterT' ('pass' m) = 'liftM' (\\ ((a, f), w) -> (a, f w)) ('runWriterT' m)@
pass :: (Monoid w, Monoid w', Monad m) => WriterT w m (a, w -> w') -> WriterT w' m a
pass m = WriterT $ \w -> do
  ((a, f), w') <- runWriterT m
  let wt = w `mappend` f w'
  wt `seq` return (a, wt)
{-# INLINE pass #-}

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\ x -> (x,f)) m)@
--
-- * @'runWriterT' ('censor' f m) = 'liftM' (\\ (a, w) -> (a, f w)) ('runWriterT' m)@
censor :: (Monoid w, Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ \w -> do
  (a, w') <- runWriterT m
  let wt = w `mappend` f w'
  wt `seq` return (a, wt)
{-# INLINE censor #-}

-- | Uniform lifting of a @callCC@ operation to the new monad.
-- This version rolls back to the original state on entering the
-- continuation.
liftCallCC :: CallCC m (a, w) (b, w) -> CallCC (WriterT w m) a b
liftCallCC callCC f = WriterT $ \w ->
  callCC $ \c -> unWriterT (f (\a -> WriterT $ \_ -> c (a, w))) w
{-# INLINE liftCallCC #-}

-- | In-situ lifting of a @callCC@ operation to the new monad.
-- This version uses the current state on entering the continuation.
-- It does not satisfy the uniformity property (see "Control.Monad.Signatures").
liftCallCC' :: CallCC m (a, w) (b, w) -> CallCC (WriterT w m) a b
liftCallCC' callCC f = WriterT $ \w ->
  callCC $ \c -> unWriterT (f (\a -> WriterT $ \w' -> c (a, w'))) w
{-# INLINE liftCallCC' #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (a, w) -> Catch e (WriterT w m) a
liftCatch catchE m h = WriterT $ \w -> unWriterT m w `catchE` \e -> unWriterT (h e) w
{-# INLINE liftCatch #-}
