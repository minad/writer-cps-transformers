{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
module Control.Monad.Trans.RWS.CPS (
  -- * The RWS monda
  RWS,
  rws,
  runRWS,
  evalRWS,
  execRWS,
  mapRWS,
  withRWS,
  -- * The RWST monad transformer
  RWST,
  runRWST,
  evalRWST,
  execRWST,
  mapRWST,
  withRWST,
  -- * Reader operations
  reader,
  ask,
  local,
  asks,
  -- * Writer operations
  writer,
  tell,
  listen,
  listens,
  pass,
  censor,
  -- * State operations
  state,
  get,
  put,
  -- modify,
  -- gets
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif

-- | A monad containing an environment of type @r@, output of type @w@
-- and an updatable state of type @s@.
type RWS r w s = RWST r w s Identity

-- | Construct an RWS computation from a function.
-- (The inverse of 'runRWS'.)
rws :: Monoid w => (r -> s -> (a, s, w)) -> RWS r w s a
rws f = RWST (\r s w -> let (a, s', w') = f r s; wt = w `mappend` w' in wt `seq` pure (a, s', wt))
{-# INLINE rws #-}

-- | Unwrap an RWS computation as a function.
-- (The inverse of 'rws'.)
runRWS :: Monoid w => RWS r w s a -> r -> s -> (a, s, w)
runRWS m r s = runIdentity (runRWST m r s)
{-# INLINE runRWS #-}

-- | Evaluate a computation with the given initial state and environment,
-- returning the final value and output, discarding the final state.
evalRWS :: Monoid w
        => RWS r w s a  -- ^RWS computation to execute
        -> r            -- ^initial environment
        -> s            -- ^initial value
        -> (a, w)       -- ^final value and output
evalRWS m r s = let
    (a, _, w) = runRWS m r s
    in (a, w)
{-# INLINE evalRWS #-}

-- | Evaluate a computation with the given initial state and environment,
-- returning the final state and output, discarding the final value.
execRWS :: Monoid w
        => RWS r w s a  -- ^RWS computation to execute
        -> r            -- ^initial environment
        -> s            -- ^initial value
        -> (s, w)       -- ^final state and output
execRWS m r s = let
    (_, s', w) = runRWS m r s
    in (s', w)
{-# INLINE execRWS #-}

-- | Map the return value, final state and output of a computation using
-- the given function.
--
-- * @'runRWS' ('mapRWS' f m) r s = f ('runRWS' m r s)@
mapRWS :: (Monoid w, Monoid w') => ((a, s, w) -> (b, s, w')) -> RWS r w s a -> RWS r w' s b
mapRWS f = mapRWST (Identity . f . runIdentity)
{-# INLINE mapRWS #-}

-- | @'withRWS' f m@ executes action @m@ with an initial environment
-- and state modified by applying @f@.
--
-- * @'runRWS' ('withRWS' f m) r s = 'uncurry' ('runRWS' m) (f r s)@
withRWS :: (r' -> s -> (r, s)) -> RWS r w s a -> RWS r' w s a
withRWS = withRWST
{-# INLINE withRWS #-}

-- ---------------------------------------------------------------------------
-- | A monad transformer adding reading an environment of type @r@,
-- collecting an output of type @w@ and updating a state of type @s@
-- to an inner monad @m@.
newtype RWST r w s m a = RWST { unRWST :: r -> s -> w -> m (a, s, w) }

-- | Unwrap an RWST computation as a function.
runRWST :: Monoid w => RWST r w s m a -> r -> s -> m (a, s, w)
runRWST m r s = unRWST m r s mempty
{-# INLINE runRWST #-}

-- | Evaluate a computation with the given initial state and environment,
-- returning the final value and output, discarding the final state.
evalRWST :: (Functor m, Monoid w)
         => RWST r w s m a      -- ^computation to execute
         -> r                   -- ^initial environment
         -> s                   -- ^initial value
         -> m (a, w)            -- ^computation yielding final value and output
evalRWST m r s = (\(a, _, w) -> (a, w)) <$> runRWST m r s
{-# INLINE evalRWST #-}

-- | Evaluate a computation with the given initial state and environment,
-- returning the final state and output, discarding the final value.
execRWST :: (Functor m, Monoid w)
         => RWST r w s m a      -- ^computation to execute
         -> r                   -- ^initial environment
         -> s                   -- ^initial value
         -> m (s, w)            -- ^computation yielding final state and output
execRWST m r s = (\(_, s', w) -> (s', w)) <$> runRWST m r s
{-# INLINE execRWST #-}

-- | Map the inner computation using the given function.
--
-- * @'runRWST' ('mapRWST' f m) r s = f ('runRWST' m r s)@
--mapRWST :: (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
mapRWST :: (Functor n, Monoid w, Monoid w')
  => (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
mapRWST f m = RWST $ \r s w -> (\(a, s', w') -> let wt = w `mappend` w'
                                                in wt `seq` (a, s', wt)) <$> f (runRWST m r s)
{-# INLINE mapRWST #-}

-- | @'withRWST' f m@ executes action @m@ with an initial environment
-- and state modified by applying @f@.
--
-- * @'runRWST' ('withRWST' f m) r s = 'uncurry' ('runRWST' m) (f r s)@
withRWST :: (r' -> s -> (r, s)) -> RWST r w s m a -> RWST r' w s m a
withRWST f m = RWST $ \r s -> uncurry (unRWST m) (f r s)
{-# INLINE withRWST #-}

instance Functor m => Functor (RWST r w s m) where
  fmap f m = RWST $ \r s w -> (\(a, s', w') -> (f a, s', w')) <$> unRWST m r s w
  {-# INLINE fmap #-}

instance Monad m => Applicative (RWST r w s m) where
  pure a = RWST $ \_ s w -> pure (a, s, w)
  {-# INLINE pure #-}

  RWST mf <*> RWST mx = RWST $ \r s w -> do
    (f, s', w')  <- mf r s w
    (x, s'', w'') <- mx r s' w'
    pure (f x, s'', w'')
  {-# INLINE (<*>) #-}

instance MonadPlus m => Alternative (RWST r w s m) where
  empty = RWST $ \_ _ _ -> mzero
  {-# INLINE empty #-}

  RWST m <|> RWST n = RWST $ \r s w -> m r s w `mplus` n r s w
  {-# INLINE (<|>) #-}

instance Monad m => Monad (RWST r w s m) where
#if !(MIN_VERSION_base(4,8,0))
  return a = RWST $ \_ s w -> pure (a, s, w)
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

instance MonadPlus m => MonadPlus (RWST r w s m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadFix m => MonadFix (RWST r w s m) where
  mfix f = RWST $ \r s w -> mfix $ \ ~(a, _, _) -> unRWST (f a) r s w
  {-# INLINE mfix #-}

instance MonadTrans (RWST r w s) where
  lift m = RWST $ \_ s w -> (\a -> (a,s,w)) <$> m
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (RWST r w s m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}
-- ---------------------------------------------------------------------------
-- Reader operations

-- | Constructor for computations in the reader monad (equivalent to 'asks').
reader :: Applicative m => (r -> a) -> RWST r w s m a
reader = asks
{-# INLINE reader #-}

-- | Fetch the value of the environment.
ask :: Applicative m => RWST r w s m r
ask = asks id
{-# INLINE ask #-}

-- | Execute a computation in a modified environment
--
-- * @'runRWST' ('local' f m) r s = 'runRWST' m (f r) s@
local :: (r -> r) -> RWST r w s m a -> RWST r w s m a
local f m = RWST $ \r s w -> unRWST m (f r) s w
{-# INLINE local #-}

-- | Retrieve a function of the current environment.
--
-- * @'asks' f = 'liftM' f 'ask'@
asks :: Applicative m => (r -> a) -> RWST r w s m a
asks f = RWST $ \r s w -> pure (f r, s, w)
{-# INLINE asks #-}

-- ---------------------------------------------------------------------------
-- Writer operations

-- | Construct a writer computation from a (result, output) pair.
writer :: (Monoid w, Applicative m) => (a, w) -> RWST r w s m a
writer (a, w') = RWST $ \_ s w -> let wt = w `mappend` w' in wt `seq` pure (a, s, wt)
{-# INLINE writer #-}

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monoid w, Applicative m) => w -> RWST r w s m ()
tell w' = writer ((), w')
{-# INLINE tell #-}

-- | @'listen' m@ is an action that executes the action @m@ and adds its
-- output to the value of the computation.
--
-- * @'runRWST' ('listen' m) r s = 'liftM' (\\ (a, w) -> ((a, w), w)) ('runRWST' m r s)@
listen :: (Monoid w, Functor m) => RWST r w s m a -> RWST r w s m (a, w)
listen = listens id
{-# INLINE listen #-}

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
--
-- * @'runRWST' ('listens' f m) r s = 'liftM' (\\ (a, w) -> ((a, f w), w)) ('runRWST' m r s)@
listens :: (Monoid w, Functor m) => (w -> b) -> RWST r w s m a -> RWST r w s m (a, b)
listens f m = RWST $ \r s w ->
  (\(a, s', w') ->
     let wt = w `mappend` w'
     in wt `seq` ((a, f w'), s', wt)) <$> runRWST m r s
{-# INLINE listens #-}

-- | @'pass' m@ is an action that executes the action @m@, which returns
-- a value and a function, and returns the value, applying the function
-- to the output.
--
-- * @'runRWST' ('pass' m) r s = 'liftM' (\\ ((a, f), w) -> (a, f w)) ('runRWST' m r s)@
pass :: (Monoid w, Monoid w', Functor m) => RWST r w s m (a, w -> w') -> RWST r w' s m a
pass m = RWST $ \r s w ->
  (\((a, f), s', w') ->
     let wt = w `mappend` f w'
     in wt `seq` (a, s', wt)) <$> runRWST m r s
{-# INLINE pass #-}

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\ x -> (x,f)) m)@
--
-- * @'runRWST' ('censor' f m) r s = 'liftM' (\\ (a, w) -> (a, f w)) ('runRWST' m r s)@
censor :: (Monoid w, Functor m) => (w -> w) -> RWST r w s m a -> RWST r w s m a
censor f m = RWST $ \r s w ->
  (\(a, s', w') ->
     let wt = w `mappend` f w'
     in wt `seq` (a, s', wt)) <$> runRWST m r s
{-# INLINE censor #-}

-- ---------------------------------------------------------------------------
-- State operations

-- | Construct a state monad computation from a state transformer function.
state :: Applicative m => (s -> (a, s)) -> RWST r w s m a
state f = RWST $ \_ s w -> let (a, s') = f s in pure (a, s', w)
{-# INLINE state #-}

-- | Fetch the current value of the state within the monad.
get :: Applicative m => RWST r w s m s
get = gets id
{-# INLINE get #-}

-- | @'put' s@ sets the state within the monad to @s@.
put :: Applicative m => s -> RWST r w s m ()
put s = RWST $ \_ _ w -> pure ((), s, w)
{-# INLINE put #-}

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
--
-- * @'modify' f = 'get' >>= ('put' . f)@
modify :: Applicative m => (s -> s) -> RWST r w s m ()
modify f = RWST $ \_ s w -> pure ((), f s, w)
{-# INLINE modify #-}

-- | Get a specific component of the state, using a projection function
-- supplied.
--
-- * @'gets' f = 'liftM' f 'get'@
gets :: Applicative m => (s -> a) -> RWST r w s m a
gets f = RWST $ \ _ s w -> pure (f s, s, w)
{-# INLINE gets #-}
