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

import Control.Monad.Trans.Writer.CPS.Internal
