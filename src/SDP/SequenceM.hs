{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds, BangPatterns #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.SequenceM
    Copyright   :  (c) Andrey Mulik 2019-2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.SequenceM" is a module that provides several convenient interfaces for
    working with various linear data structures.
-}
module SDP.SequenceM
(
  -- * Exports
  module SDP.Linear,
  
  -- * SequenceM class
  SequenceM (..), SequenceM1, SequenceM2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  SequenceM', SequenceM'',
#endif
  
  -- ** Extra functions
  prefixM, suffixM
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear

default ()

--------------------------------------------------------------------------------

class NullableM m l => SequenceM m l e | l -> m, l -> e
  where
    {-# MINIMAL ofoldrM, ofoldlM #-}
    
    {- Folds with offset. -}
    
    -- | 'ofoldrM' is right monadic fold with offset.
    ofoldrM :: (Int -> e -> r -> m r) -> r -> l -> m r
    
    -- | 'ofoldlM' is left monadic fold with offset.
    ofoldlM :: (Int -> r -> e -> m r) -> r -> l -> m r
    
    {- Folds. -}
    
    -- | 'foldrM' is just 'ofoldrM' in 'Linear' context.
    foldrM :: (e -> r -> m r) -> r -> l -> m r
    foldrM =  ofoldrM . const
    
    -- | 'foldlM' is just 'ofoldlM' in 'Linear' context.
    foldlM :: (r -> e -> m r) -> r -> l -> m r
    foldlM =  ofoldlM . const
    
    -- | 'foldrM1' is 'foldrM' version with 'last' element as base.
    foldrM1 :: MonadFail m => (e -> e -> m e) -> l -> m e
    foldrM1 go es = getLeft es >>= \ es' -> case es' of
      x : xs -> foldr (\ e acc -> go e =<< acc) (return x) xs
      _      -> fail "foldrM1: expected non-empty structure"
    
    -- | 'foldlM1' is 'foldlM' version with 'head' element as base.
    foldlM1 :: MonadFail m => (e -> e -> m e) -> l -> m e
    foldlM1 go es = getLeft es >>= \ es' -> case es' of
      xs :< x -> foldl (\ acc e -> flip go e =<< acc) (return x) xs
      _       -> fail "foldrM1: expected non-empty structure"
    
    -- | Left view of line.
    {-# INLINE getLeft #-}
    getLeft :: l -> m [e]
    getLeft =  foldrM (\ e es -> return (e : es)) []
    
    -- | Right view of line.
    {-# INLINE getRight #-}
    getRight :: l -> m [e]
    getRight =  foldlM (\ es e -> return (e : es)) []
    
    {- Subsequence operations. -}
    
    -- | @mprefix p es@ returns the longest @es@ prefix size, satisfying @p@.
    mprefix :: (e -> m Bool) -> l -> m Int
    mprefix p = foldrM (\ e c -> p e ?^ pure (succ c) $ pure 0) 0
    
    -- | @msuffix p es@ returns the longest @es@ suffix size, satisfying @p@.
    msuffix :: (e -> m Bool) -> l -> m Int
    msuffix p = foldlM (\ c e -> p e ?^ pure (succ c) $ pure 0) 0

--------------------------------------------------------------------------------

-- | 'SequenceM' contraint for @(Type -> Type)@-kind types.
type SequenceM1 m l e = SequenceM m (l e) e

-- | 'SequenceM' contraint for @(Type -> Type -> Type)@-kind types.
type SequenceM2 m l i e = SequenceM m (l i e) e

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'SequenceM' contraint for @(Type -> Type)@-kind types.
type SequenceM' m l = forall e . SequenceM m (l e) e

-- | 'SequenceM' contraint for @(Type -> Type -> Type)@-kind types.
type SequenceM'' m l = forall i e . SequenceM m (l i e) e
#endif

--------------------------------------------------------------------------------

-- | @prefixM p es@ returns the longest @es@ prefix size, satisfying @p@.
prefixM :: SequenceM m l e => (e -> Bool) -> l -> m Int
prefixM =  mprefix . (pure .)

-- | @suffixM p es@ returns the longest @es@ suffix size, satisfying @p@.
suffixM :: SequenceM m l e => (e -> Bool) -> l -> m Int
suffixM =  msuffix . (pure .)


