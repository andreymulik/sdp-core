{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds, DefaultSignatures #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Estimate
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    The "SDP.Estimate" class provides the 'Estimate' and 'EstimateM' classes for
    short-circuit comparisons of the sizes of structures among themselves and
    with a known number.
    
    Saves time for lazy structs (and eternity for infinite) if all you need to
    know is "is the struct longer than 5 elements?", "are the sizes of these two
    structs equal?", or "does this struct have exactly 2 elements?".
    
    This module is exported by "SDP.SafePrelude".
-}
module SDP.Estimate
(
  -- * Exports
  module Data.Functor.Classes,
  
  -- * Estimate
  Estimate (..), Estimate1, Estimate2, SizeHint (..),
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Estimate', Estimate'',
#endif
  
  -- ** Right-side Estimate functions.
  (<=.>), (<.), (>.), (<=.), (>=.), (==.), (/=.),
  
  -- * EstimateM
  EstimateM (..), EstimateM1, EstimateM2,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  EstimateM', EstimateM''
#endif
)
where

import SDP.Internal.Utils
import SDP.Comparing
import SDP.Index

import Data.Functor.Classes

default ()

infixl 4 <==>, .<., .>., .<=., .>=., .==., ./=.,
         <.=>, .<, .>, .<=, .>=, .==, ./=,
         <=.>, <., >., <=., >=., ==., /=.

infixl 4 <<=>>, `shorterThanM`, `longerThanM`, `hasLengthM`, `otherLengthM`,
         <=>>,  `noLongerThanM`, `notShorterThanM`

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  * @'SizeHint' l u@ - size of structure is between @l@ and @u@
  * @'SizeHintEQ' n@ - size of structure is strictly equal to @n@
  * @'SizeHintLE' l@ - size of structure is lesser or equal than @l@
  * @'SizeHintGE' u@ - size of structure is greater or equal than @u@
  
  Rules:
  
  @
  SizeHint l u <- sizeHint es === es .>= l && es .<= u
  SizeHintEQ n <- sizeHint es === es .== n
  SizeHintLE l <- sizeHint es === es .<= l
  SizeHintGE u <- sizeHint es === es .>= u
  @
-}
data SizeHint = SizeHint   {-# UNPACK #-} !Int {-# UNPACK #-} !Int
              | SizeHintEQ {-# UNPACK #-} !Int
              | SizeHintLE {-# UNPACK #-} !Int
              | SizeHintGE {-# UNPACK #-} !Int
  deriving ( Eq, Show, Read )

--------------------------------------------------------------------------------

{- |
  'Estimate' class provides the lazy comparsion structures by length.
  
  For some types (e.g., lists), this allows you to speed up the comparison or
  make it finite. For others (e.g., arrays), it may be convenient abbreviation.
-}
class Estimate e
  where
    {-# MINIMAL sizeOf, (<.=>), (<==>) #-}
    
    -- | Faster, but less precise version of 'SDP.Bordered.Bordered.sizeOf'.
    sizeHint :: e -> Maybe SizeHint
    sizeHint =  const Z
    
    -- | Returns actual size of structure.
    sizeOf :: e -> Int
    
    -- | Compare structure length with given number.
    (<.=>) :: e -> Int -> Ordering
    
    -- | Compare pair of structures by length.
    (<==>) :: Compare e
    
    -- | Compare structure length with given number.
    (.==), (./=), (.<=), (.>=), (.<), (.>) :: e -> Int -> Bool
    
    -- | Compare pair of structures by length.
    (.<.), (.>.), (.<=.), (.>=.), (.==.), (./=.) :: e -> e -> Bool
    
    e .<  i = case e <.=> i of {LT -> True; _ -> False}
    e .>  i = case e <.=> i of {GT -> True; _ -> False}
    e .<= i = case e <.=> i of {GT -> False; _ -> True}
    e .>= i = case e <.=> i of {LT -> False; _ -> True}
    e .== i = case e <.=> i of {EQ -> True; _ -> False}
    e ./= i = case e <.=> i of {EQ -> False; _ -> True}
    
    e1 .<.  e2 = case e1 <==> e2 of {LT -> True; _ -> False}
    e1 .>.  e2 = case e1 <==> e2 of {GT -> True; _ -> False}
    e1 .<=. e2 = case e1 <==> e2 of {GT -> False; _ -> True}
    e1 .>=. e2 = case e1 <==> e2 of {LT -> False; _ -> True}
    e1 .==. e2 = case e1 <==> e2 of {EQ -> True; _ -> False}
    e1 ./=. e2 = case e1 <==> e2 of {EQ -> False; _ -> True}

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'EstimateM' class provides the lazy comparsion structures by length.
  
  For some types (e.g., lists), this allows you to speed up the comparison or
  make it finite. For others (e.g., arrays), it may be convenient abbreviation.
-}
class Monad m => EstimateM m e
  where
    getSizeHint :: e -> m (Maybe SizeHint)
    getSizeHint =  const $ return Z
    
    -- | 'getSizeOf' returns size'of mutable data structure.
    default getSizeOf :: Estimate e => e -> m Int
    getSizeOf :: e -> m Int
    getSizeOf =  return . sizeOf
    
    -- | Compare pair of structures by length.
    default (<<=>>) :: Estimate e => e -> e -> m Ordering
    (<<=>>) :: e -> e -> m Ordering
    (<<=>>) =  return ... (<==>)
    
    -- | Compare structure length with given number.
    default (<=>>) :: Estimate e => e -> Int -> m Ordering
    (<=>>) :: e -> Int -> m Ordering
    (<=>>) =  return ... (<.=>)
    
    -- | Compare structure length with given number.
    shorterThanM, longerThanM, noLongerThanM, notShorterThanM,
      hasLengthM, otherLengthM :: e -> Int -> m Bool
    
    shorterThanM    e i = fmap (== LT) (e <=>> i)
    longerThanM     e i = fmap (== GT) (e <=>> i)
    noLongerThanM   e i = fmap (/= GT) (e <=>> i)
    notShorterThanM e i = fmap (/= LT) (e <=>> i)
    hasLengthM      e i = fmap (== EQ) (e <=>> i)
    otherLengthM    e i = fmap (/= EQ) (e <=>> i)
    
    -- | Compare pair of structures by length.
    estimateMLT, estimateMGT, estimateMLE, estimateMGE,
      estimateMEQ, estimateMNE :: e -> e -> m Bool
    
    estimateMLT e1 e2 = fmap (== LT) (e1 <<=>> e2)
    estimateMGT e1 e2 = fmap (== GT) (e1 <<=>> e2)
    estimateMLE e1 e2 = fmap (/= GT) (e1 <<=>> e2)
    estimateMGE e1 e2 = fmap (/= LT) (e1 <<=>> e2)
    estimateMEQ e1 e2 = fmap (== EQ) (e1 <<=>> e2)
    estimateMNE e1 e2 = fmap (/= EQ) (e1 <<=>> e2)

--------------------------------------------------------------------------------

-- | @(Type -> Type)@ kind 'Estimate'.
type Estimate1 rep e = Estimate (rep e)

-- | @(Type -> Type -> Type)@ kind 'Estimate'.
type Estimate2 rep i e = Estimate (rep i e)

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Estimate' quantified contraint for @(Type -> Type)@-kind types.
type Estimate' rep = forall e . Estimate (rep e)

-- | 'Estimate' quantified contraint for @(Type -> Type -> Type)@-kind types.
type Estimate'' rep = forall i e . Estimate (rep i e)
#endif

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @(Type -> Type)@ kind 'EstimateM'.
-}
type EstimateM1 m rep e = EstimateM m (rep e)

{- |
  @since 0.3
  
  @(Type -> Type -> Type)@ kind 'EstimateM'.
-}
type EstimateM2 m rep i e = EstimateM m (rep i e)

#ifdef SDP_QUALIFIED_CONSTRAINTS
{- |
  @since 0.3
  
  'EstimateM' quantified contraint for @(Type -> Type)@-kind types.
-}
type EstimateM' m rep = forall e . EstimateM m (rep e)

{- |
  @since 0.3
  
  'EstimateM' quantified contraint for @(Type -> Type -> Type)@-kind types.
-}
type EstimateM'' m rep = forall i e . EstimateM m (rep i e)
#endif

--------------------------------------------------------------------------------

-- | Compare given number with structure length.
(<=.>) :: Estimate e => Int -> e -> Ordering
i <=.> e = case e <.=> i of {LT -> GT; EQ -> EQ; GT -> LT}

-- | Compare given number with structure length.
(==.), (/=.), (<=.), (>=.), (<.), (>.) :: Estimate e => Int -> e -> Bool

(==.) = flip (.==)
(/=.) = flip (./=)
(<=.) = flip (.>=)
(>=.) = flip (.<=)
(<.)  = flip (.>)
(>.)  = flip (.<)

--------------------------------------------------------------------------------

instance Index i => Estimate (i, i)
  where
    sizeHint = Just . SizeHintEQ . size
    sizeOf   = size
    
    (<==>) = on (<=>) size
    (.<=.) = on (<=)  size
    (.>=.) = on (>=)  size
    (.>.)  = on (>)   size
    (.<.)  = on (<)   size
    
    (<.=>) = (<=>) . size
    (.<=)  = (<=)  . size
    (.>=)  = (>=)  . size
    (.>)   = (>)   . size
    (.<)   = (<)   . size

instance (Monad m, Index i) => EstimateM m (i, i)
  where
    getSizeOf   = return . size
    getSizeHint = return . Just . SizeHintEQ . size
    
    (<<=>>) = return ... on (<=>) size
    (<=>>)  = return ... (<=>) . size
    
    estimateMEQ = return ... on (==)  size
    estimateMLE = return ... on (<=)  size
    estimateMGE = return ... on (>=)  size
    estimateMLT = return ... on (<)   size
    estimateMGT = return ... on (>)   size
    
    hasLengthM      = return ... (==)  . size
    otherLengthM    = return ... (/=)  . size
    longerThanM     = return ... (>)   . size
    shorterThanM    = return ... (<)   . size
    noLongerThanM   = return ... (<=)  . size
    notShorterThanM = return ... (>=)  . size

--------------------------------------------------------------------------------

instance Estimate [a]
  where
    sizeOf = length
    
    sizeHint [] = Just (SizeHintEQ 0)
    sizeHint  _ = Just (SizeHintGE 1)
    
    [] <==> [] = EQ
    [] <==>  _ = LT
    _  <==> [] = GT
    (_ : xs) <==> (_ : ys) = xs <==> ys
    
    [] <.=> n = 0 <=> n
    es <.=> n =
      let
          go _   0 = GT
          go xs' c = case xs' of {_ : xs -> go xs (c - 1); _ -> 0 <=> c}
      in  if n < 0 then LT else go es n

instance Monad m => EstimateM m [a]
  where
    getSizeOf   = return . length
    getSizeHint = return . sizeHint
    
    (<<=>>) = return ... (<==>)
    (<=>>)  = return ... (<.=>)




