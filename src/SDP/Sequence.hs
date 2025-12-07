{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds, BangPatterns #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Sequence
    Copyright   :  (c) Andrey Mulik 2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Sequence" is a module that provides several convenient interfaces for
    working with various linear data structures.
-}
module SDP.Sequence
(
  -- * Exports
  module SDP.Nullable,
  
  -- * Sequence class
  Sequence (..), Sequence1, Sequence2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Sequence', Sequence'',
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Nullable

import Data.List ( reverse )

default ()

--------------------------------------------------------------------------------

class Nullable l => Sequence l e | l -> e
  where
    {-# MINIMAL ofoldr, ofoldl #-}
    
    {- Folds with offset. -}
    
    -- | 'ofoldr' is right fold with offset.
    ofoldr :: (Int -> e -> b -> b) -> b -> l -> b
    ofoldr f base = ofoldr f base . listL
    
    -- | 'ofoldl' is left fold with offset.
    ofoldl :: (Int -> b -> e -> b) -> b -> l -> b
    ofoldl f base = ofoldl f base . listL
    
    -- | 'ofoldr'' is strict version of 'ofoldr'.
    ofoldr' :: (Int -> e -> b -> b) -> b -> l -> b
    ofoldr' f = ofoldr (\ !i e !b -> f i e b)
    
    -- | 'ofoldl'' is strict version of 'ofoldl'.
    ofoldl' :: (Int -> b -> e -> b) -> b -> l -> b
    ofoldl' f = ofoldl (\ !i !b e -> f i b e)
    
    {- Folds. -}
    
    -- | 'sfoldr' is just 'foldr' in 'Linear' context.
    sfoldr :: (e -> b -> b) -> b -> l -> b
    sfoldr =  ofoldr . const
    
    -- | 'sfoldl' is just 'foldl' in 'Linear' context.
    sfoldl :: (b -> e -> b) -> b -> l -> b
    sfoldl =  ofoldl . const
    
    -- | 'sfoldr'' is just 'foldr'' in 'Linear' context.
    sfoldr' :: (e -> b -> b) -> b -> l -> b
    sfoldr' =  ofoldr' . const
    
    -- | 'sfoldl'' is just 'foldl'' in 'Linear' context.
    sfoldl' :: (b -> e -> b) -> b -> l -> b
    sfoldl' =  ofoldl' . const
    
    -- | Left to right view of line, same to 'toList'.
    listL :: l -> [e]
    listL =  sfoldr (:) []
    
    -- | Right to left view of line.
    listR :: l -> [e]
    listR =  sfoldl (flip (:)) []
    
    -- | prefix gives length of init, satisfying preducate.
    prefix :: (e -> Bool) -> l -> Int
    prefix p = sfoldr' (\ e c -> p e ? succ c $ 0) 0
    
    -- | suffix gives length of tail, satisfying predicate.
    suffix :: (e -> Bool) -> l -> Int
    suffix p = sfoldl' (\ c e -> p e ? succ c $ 0) 0

--------------------------------------------------------------------------------

{- Constraint types. -}

-- | 'Sequence' contraint for @(Type -> Type)@-kind types.
type Sequence1 l e = Sequence (l e) e

-- | 'Sequence' contraint for @(Type -> Type -> Type)@-kind types.
type Sequence2 l i e = Sequence (l i e) e

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Sequence' contraint for @(Type -> Type)@-kind types.
type Sequence' l = forall e . Sequence (l e) e

-- | 'Sequence' contraint for @(Type -> Type -> Type)@-kind types.
type Sequence'' l = forall i e . Sequence2 l i e
#endif

--------------------------------------------------------------------------------

instance Sequence [e] e
  where
    ofoldr f base =
      let go !i es = case es of {x : xs -> f i x $ go (i + 1) xs; _ -> base}
      in  go 0
    
    ofoldl f =
      let go !i base es = case es of {x : xs -> go (i + 1) (f i base x) xs; _ -> base}
      in  go 0
    
    sfoldr' = foldr'
    sfoldl' = foldl'
    sfoldr  = foldr
    sfoldl  = foldl
    
    listL = toList
    listR = reverse




