{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.RWS.CPS
-- Copyright   :  (c) Daniel Mendler 2016,
--                (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- A monad transformer that combines 'ReaderT', 'WriterT' and 'StateT'.
-- This version uses continuation-passing-style for the writer part
-- to achieve constant space usage. This transformer can be used as a
-- drop-in replacement for "Control.Monad.Trans.RWS.Strict".
-----------------------------------------------------------------------------

module Control.Monad.Trans.RWS.CPS (
  -- * The RWS monad
  RWS,
  rws,
  runRWS,
  evalRWS,
  execRWS,
  mapRWS,
  withRWS,
  -- * The RWST monad transformer
  RWST,
  rwsT,
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
  modify,
  gets,
  -- * Lifting other operations
  liftCallCC,
  liftCallCC',
  liftCatch,
) where

import Control.Monad.Trans.RWS.CPS.Internal
