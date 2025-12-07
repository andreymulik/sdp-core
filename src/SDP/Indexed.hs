{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, FlexibleContexts, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
{-# LANGUAGE DefaultSignatures, GADTs, CPP #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Indexed
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
  "SDP.Indexed" provides 'Indexed' and 'Freeze' classes.
-}
module SDP.Indexed
(
  -- * Exports
  module SDP.Linear,
  module SDP.Map,
  
  -- * Indexed
  Indexed (..), Indexed1, Indexed2,
  
  -- * Freeze
  Freeze (..), Freeze1, Freeze2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Indexed', Indexed'', Freeze', Freeze''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear
import SDP.Map

import GHC.Exts
import GHC.ST

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'Indexed' is class of ordered associative arrays with static bounds.
class (Linear v e, Bordered v i, Map v i e) => Indexed v i e | v -> i e
  where
    {-# MINIMAL assoc', fromIndexed #-}
    
    {- |
      @assoc bnds ascs@ create new structure from list of @(key, value)@ pairs,
      without default element. Note that @bnds@ is @ascs@ bounds and may not
      match with the result bounds (not always possible).
    -}
    assoc :: (i, i) -> [(i, e)] -> v
    assoc bnds = toMap . filter (inRange bnds . fst)
    
    {- |
      @assoc' def bnds ascs@ creates new structure from list of associations
      with default element. Note that @bnds@ is @ascs@ bounds and may not match
      with the result bounds (not always possible).
    -}
    assoc' :: e -> (i, i) -> [(i, e)] -> v
    
    -- | 'fromIndexed' converts this indexed structure to another one.
    fromIndexed :: Indexed m j e => m -> v
    
    -- | 'reshape' creates new structure with index mapping.
    reshape :: Indexed src j e => (i, i) -> src -> (i -> j) -> v
    reshape bnds es f = assoc bnds [ (i, es!j) | i <- range bnds, let j = f i ]
    
    {- |
      @'accum' f es ies@ create a new structure from @es@ elements selectively
      updated by function @f@ and @ies@ associations list.
    -}
    accum :: (e -> e' -> e) -> v -> [(i, e')] -> v
    accum f es ascs = bounds es `assoc` [ (i, es!i `f` e') | (i, e') <- ascs ]
    
    {- |
      @since 0.3
      
      @'subshapeOf' es ij@ returns subshape @ij@ of @es@.
    -}
    subshapeOf :: (Indexed2 s i e, Indexed2 s j e, SubIndex i j)
               => v -> i :|: j -> s j e
    
    subshapeOf es ij = toMap $ kfoldr (\ i e ies ->
        let (ij', j) = splitDim i
        in  ij' == ij ? (j, e) : ies $ ies
      ) [] es
    
    {- |
      @since 0.3
      
      Returns list of @es@ subshapes.
    -}
    slicesOf :: (Indexed2 s (i :|: j) (s j e), Indexed2 s j e, SubIndex i j)
             => v -> s (i :|: j) (s j e)
    
    slicesOf es = let ((ls, l), (us, u)) = splitDim `both` bounds es in assoc (ls, us)
        [
          (ij, assoc (l, u) ies)
        | (ij, ies) <- size (ls, us) `unpick` offset (ls, us) $ kfoldr
          (\ i e xs -> flip (,) e `second` splitDim i : xs) [] es
        ]
    
    {- |
      @since 0.3
      
      Unslice subshapes.
    -}
    unslice :: (Indexed2 s (i :|: j) (s j e), Indexed2 s j e, SubIndex i j)
            => s (i :|: j) (s j e) -> v
    
    unslice = toMap . kfoldr (\ i -> (++) . (first (`joinDim` i) <$>) . assocs) []

--------------------------------------------------------------------------------

instance Indexed [e] Int e
  where
    assoc' e _ = snds . fill . setWith cmpfst
      where
        fill (ix@(i1, _) : iy@(i2, _) : ies) =
          let rest = i1 + 1 == i2 ? iy : ies $ (i1 + 1, e) : iy : ies
          in  ix : fill rest
        fill xs = xs
    
    fromIndexed es = (es !) <$> indices es

--------------------------------------------------------------------------------

-- | Service class of mutable to immutable conversions.
class Monad m => Freeze m v' v | v' -> m
  where
    {- |
      @freeze@ is a safe way to convert a mutable structure to a immutable.
      @freeze@ should copy the old structure or ensure that it will not be used
      after calling the procedure.
    -}
    freeze :: v' -> m v
    
    {- |
      @unsafeFreeze@ is unsafe version of 'freeze'. @unsafeFreeze@ doesn't
      guarantee that the structure will be copied or locked. It only guarantees
      that if the old structure isn't used, no error will occur.
    -}
    unsafeFreeze :: v' -> m v
    unsafeFreeze =  freeze

--------------------------------------------------------------------------------

-- | 'Indexed' contraint for @(Type -> Type)@-kind types.
type Indexed1 v i e = Indexed (v e) i e

-- | 'Indexed' contraint for @(Type -> Type -> Type)@-kind types.
type Indexed2 v i e = Indexed (v i e) i e

-- | 'Freeze' contraint for @(Type -> Type)@-kind types.
type Freeze1 m v' v e = Freeze m (v' e) (v e)

-- | 'Freeze' contraint for @(Type -> Type -> Type)@-kind types.
type Freeze2 m v' v i e = Freeze m (v' i e) (v i e)

--------------------------------------------------------------------------------

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Indexed' contraint for @(Type -> Type)@-kind types.
type Indexed' v i = forall e . Indexed (v e) i e

-- | 'Indexed' contraint for @(Type -> Type -> Type)@-kind types.
type Indexed'' v = forall i e . Indexed (v i e) i e

-- | 'Freeze' contraint for @(Type -> Type)@-kind types.
type Freeze' m v' v = forall e . Freeze m (v' e) (v e)

-- | 'Freeze' contraint for @(Type -> Type -> Type)@-kind types.
type Freeze'' m v' v = forall i e . Freeze m (v' i e) (v i e)
#endif

--------------------------------------------------------------------------------

-- | Split the list and return the resulting chunks.
unpick :: Int -> (i -> Int) -> [(i, e)] -> [(i, [e])]
unpick (I# n#) f es = runST $ ST $ \ s1# -> case newArray# n# [] s1# of
  (# s2#, marr# #) -> case newArray# n# (undEx "unpick") s2# of
    (# s3#, midx# #) -> case foldr (\ (i, e) go s# -> case f i of
      I# i# -> case readArray# marr# i# s# of
        (# s', xs #) -> case writeArray# marr# i# (e : xs) s' of
          s''# -> case writeArray# midx# i# i s''# of
            s'''# -> go s'''#
      ) (\ s# -> s#) es s3# of
        s4# -> case unsafeFreezeArray# marr# s4# of
          (# s5#, arr# #) -> case unsafeFreezeArray# midx# s5# of
            (# s6#, idx# #) ->
              let
                  go -1# xss = xss
                  go  c# xss =
                    let (# xs #) = indexArray# arr# c#
                        (# i #)  = indexArray# idx# c#
                    in  go (c# -# 1#) ((i, reverse xs) : xss)
              in  (# s6#, go (n# -# 1#) [] #)

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Indexed."



