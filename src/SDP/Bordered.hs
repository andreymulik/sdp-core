{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Bordered
    Copyright   :  (c) Andrey Mulik 2021-2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Bordered" is a module that provides 'Bordered' and 'BorderedM' classes
    of structures with bounds.
    
    @since 0.3
-}
module SDP.Bordered
(
  -- * Bordered
  -- $terminology
  Bordered (..), Bordered1, Bordered2, viewOf, unsafeViewOf,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Bordered', Bordered'',
#endif
  
  -- * Monadic Bordered
  BorderedM (..), BorderedM1, BorderedM2, unsafeGetViewOf, getViewOf,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  BorderedM', BorderedM''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Index

import qualified Data.List as L
import Data.Functor

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- $terminology
  The following terminology is used here and below.
  
  Actual size is the number of elements actually stored in the data structure.
  
  Virtual size, or simply size, is the declared size of the structure, which,
  depending on the implementation, can be:
  * strictly equal to the actual size (e.g., lists)
  * less than or equal to the actual size (Vector, ByteString, all standard SDP structures)
  * greater than or equal to the actual size (e.g., sparse matrices)
  
  All SDP classes work only with the virtual size (including Unboxed, unless
  otherwise noted).
-}

--------------------------------------------------------------------------------

{- |
  'Bordered' is a class of structures that have a constant size and borders
  (represented by the smallest and largest indices).
  
  Note that the 'Bordered' class isn't designed for structures with unordered or
  missing keys. Its main purpose is to determine the geometry ('sizesOf'), the
  order of the keys ('indexOf', 'offsetOf') and ensure that the keys are
  correctly mapped to the content.
  
  'Bordered' doesn't affect the contents of the structure, but implies that each
  index within the specified bounds /uniquely/ corresponds to some stored or
  computed element. Boundary operations don't affect the order of the elements,
  but may make some of them unreachable (if the size of the structure is
  reduced) or change the corresponding indices.
  
  All 'Bordered' operations is safe. Only 'viewOf' can 'throw' exceptions.
-}
class (Index i, Estimate b) => Bordered b i | b -> i
  where
    {-# MINIMAL (bounds|(lower, upper)), eitherViewOf #-}
    
    {-# INLINE bounds #-}
    {- |
      Returns the exact 'lower' and 'upper' bounds of given structure. If the
      structure doesn't have explicitly defined boundaries (list, for example),
      use the @'defaultBounds' . 'sizeOf'@.
    -}
    bounds :: b -> (i, i)
    bounds es = (lower es, upper es)
    
    {-# INLINE lower #-}
    -- | Returns lower bound of structure
    lower :: b -> i
    lower =  fst . bounds
    
    {-# INLINE upper #-}
    -- | Returns upper bound of structure
    upper :: b -> i
    upper =  snd . bounds
    
    -- | Returns the virtual geometry of givel structure.
    sizesOf :: b -> [Int]
    sizesOf =  sizes . bounds
    
    {-# INLINE indexIn #-}
    -- | Checks if an index within the boundaries of the structure.
    indexIn :: b -> i -> Bool
    indexIn =  inRange . bounds
    
    {-# INLINE indices #-}
    -- | Returns list of indices, also see 'range'.
    indices :: b -> [i]
    indices =  range . bounds
    
    {-# INLINE indexOf #-}
    -- | Returns 'index' by 'offset'.
    indexOf :: b -> Int -> i
    indexOf =  index . bounds
    
    {-# INLINE offsetOf #-}
    -- | Returns 'offset' by 'index'.
    offsetOf :: b -> i -> Int
    offsetOf =  offset . bounds
    
    {- |
      @since 0.3
      
      'eitherViewOf' is a function that tries to enforce the specified bounds to
      given structure. If it fails, it returns error information.
      
      See 'viewOf' for everyday use or 'unsafeViewOf' in case you want to handle
      possible exceptions above.
      
      The function @'eitherViewOf' bnds es@ must work exactly like this and
      perform checks exactly in this order:
      
      * If @'isEmpty' bnds@, then the function returns an empty structure, not
        referencing @es@ and preserving laziness on the second argument
      * If the @bnds@ aren't applicable to the structure, the function returns
        'InapplicableBoundaries'
      * If @bnds@ is larger than the size of the current structure and it
        doesn't allow such an increase, then the function must return
        'UnacceptableExpansion'
      * All other 'IndexException' values ​​aren't applicable to this function
      * When shrinking @es@, the function must return the slice of first
        @size bnds@ elements of @es@ starting with @lower es@ in the order
        determined by the @next@ function.
      
      IMPORTANT: 'eitherViewOf' must not \"legalize\" access to values ​​and
      memory areas that may be outside the boundaries of the passed structure or
      implicitly expand structures by filling them with default/undefined/etc.
    -}
    eitherViewOf :: (i, i) -> b -> Either IndexException b

{- |
  @since 0.3
  
  'viewOf' is a version of 'eitherViewOf' that returns either the result or
  'Nothing'.
-}
viewOf :: Bordered b i => (i, i) -> b -> Maybe b
viewOf bnds es = case eitherViewOf bnds es of
  Right res -> Just res
  _         -> Nothing

{- |
  @since 0.3
  
  'unsafeViewOf' is a version of 'eitherViewOf' that either returns the result
  or throws the appropriate exception.
-}
unsafeViewOf :: Bordered b i => (i, i) -> b -> b
unsafeViewOf bnds es = case eitherViewOf bnds es of
  Left  err -> throw err
  Right res -> res

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'BorderedM' is 'Bordered' version for mutable data structures.
-}
class (Monad m, Index i, EstimateM m b) => BorderedM m b i | b -> i
  where
    {-# MINIMAL (getBounds|getLower, getUpper), getEitherViewOf #-}
    
    -- | 'getBounds' returns 'bounds' of mutable data structure.
    getBounds :: b -> m (i, i)
    getBounds es = liftA2 (,) (getLower es) (getUpper es)
    
    -- | 'getLower' returns 'lower' bound of mutable data structure.
    getLower :: b -> m i
    getLower =  fsts . getBounds
    
    -- | 'getUpper' returns 'upper' bound of mutable data structure.
    getUpper :: b -> m i
    getUpper =  snds . getBounds
    
    -- | 'getSizesOf' returns 'sizes' of mutable data structure.
    getSizesOf :: b -> m [Int]
    getSizesOf =  fmap sizes . getBounds
    
    -- | 'nowIndexIn' is 'indexIn' version for mutable structures.
    nowIndexIn :: b -> i -> m Bool
    nowIndexIn es i = flip inRange i <$> getBounds es
    
    -- | 'getIndices' returns 'indices' of mutable data structure.
    getIndices :: b -> m [i]
    getIndices =  fmap range . getBounds
    
    -- | 'getIndexOf' is 'indexOf' version for mutable structures.
    getIndexOf :: b -> Int -> m i
    getIndexOf es i = getBounds es <&> (`index` i)
    
    -- | 'getOffsetOf' is 'offsetOf' version for mutable structures.
    getOffsetOf :: b -> i -> m Int
    getOffsetOf es i = getBounds es <&> (`offset` i)
    
    {- |
      @since 0.3
      
      'getEitherViewOf' is a function that tries to enforce the specified bounds
      to given structure. If it fails, it returns error information.
      
      See 'getViewOf' for everyday use or 'unsafeGetViewOf' in case you want to
      handle possible exceptions above.
      
      The function @'getEitherViewOf' bnds es@ must work exactly like this and
      perform checks exactly in this order:
      
      * If @'isEmpty' bnds@, then the function returns an empty structure, not
        referencing @es@ and preserving laziness on the second argument
      * If the @bnds@ aren't applicable to the structure, the function returns
        'InapplicableBoundaries'
      * If @bnds@ is larger than the size of the current structure and it doesn't
        allow such an increase, then the function must return
        'UnacceptableExpansion'
      * All other 'IndexException' values ​​aren't applicable to this function
      * When shrinking @es@, the function must return the slice of first
        @size bnds@ elements of @es@ starting with @lower es@ in the order
        determined by the @next@ function.
      
      IMPORTANT: 'getEitherViewOf' must not \"legalize\" access to values ​​and
      memory areas that may be outside the boundaries of the passed structure or
      implicitly expand structures by filling them with default/undefined/etc.
    -}
    getEitherViewOf :: (i, i) -> b -> m (Either IndexException b)

{- |
  @since 0.3
  
  'unsafeGetViewOf' is a version of 'getEitherViewOf' that either returns
  the result or throws the appropriate exception.
-}
unsafeGetViewOf :: BorderedM m b i => (i, i) -> b -> m b
unsafeGetViewOf bnds es = do
  res' <- getEitherViewOf bnds es
  case res' of
    Left  err -> throw err
    Right res -> pure res

{- |
  @since 0.3
  
  'getViewOf' is a version of 'eitherViewOf' that returns either the result or
  'Nothing'.
-}
getViewOf :: BorderedM m b i => (i, i) -> b -> m (Maybe b)
getViewOf bnds es = do
  res' <- getEitherViewOf bnds es
  pure $ case res' of
    Right res -> Just res
    _         -> Nothing

--------------------------------------------------------------------------------

-- | 'Bordered' contraint for @(Type -> Type)@-kind types.
type Bordered1 l i e = Bordered (l e) i

-- | 'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
type Bordered2 l i e = Bordered (l i e) i

-- | 'BorderedM' contraint for @(Type -> Type)@-kind types.
type BorderedM1 m l i e = BorderedM m (l e) i

-- | 'BorderedM' contraint for @(Type -> Type -> Type)@-kind types.
type BorderedM2 m l i e = BorderedM m (l i e) i

--------------------------------------------------------------------------------

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Bordered' contraint for @(Type -> Type)@-kind types.
type Bordered' l i = forall e . Bordered (l e) i

-- | 'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
type Bordered'' l = forall i e . Bordered (l i e) i

-- | 'BorderedM' contraint for @(Type -> Type)@-kind types.
type BorderedM' m l i = forall e . BorderedM m (l e) i

-- | 'BorderedM' contraint for @(Type -> Type -> Type)@-kind types.
type BorderedM'' m l = forall i e . BorderedM m (l i e) i
#endif

--------------------------------------------------------------------------------

instance Index i => Bordered (i, i) i
  where
    bounds = id
    lower  = fst
    upper  = snd
    
    indices = range
    indexIn = inRange
    
    indexOf  = index
    offsetOf = offset
    
    eitherViewOf bnds es
        | isEmpty bnds = Right (defaultBounds 0)
        | bnds .>. es  = Left  expandEx
        |     True     = Right bnds
      where
        expandEx = UnacceptableExpansion
                 . showString "in SDP.Bordered.eitherViewOf: new borders "
                 . shows bnds . showString " can't be wider than "
                 $ shows es " (old borders) range"

instance Bordered [e] Int
  where
    lower  _ = 0
    upper es = length es - 1
    
    eitherViewOf bnds@(l, _) es
        | isEmpty bnds = Right Z
        |    l /= 0    = Left  inapplicableEx
        |    expands   = Left  expandEx
        |     True     = Right $ L.take (size bnds) es
      where
        inapplicableEx = InapplicableBoundaries
                       . showString "in SDP.Bordered.eitherViewOf: lower border "
                       $ shows l " of list should be 0"
        
        expandEx = UnacceptableExpansion
                 . showString "in SDP.Bordered.eitherViewOf: new borders "
                 $ shows bnds " can't be wider than range of list values"
        
        expands = size bnds >. es

instance (Monad m, Index i) => BorderedM m (i, i) i
  where
    getBounds  = return
    getLower   = return . fst
    getUpper   = return . snd
    getIndices = return . range
    
    getIndexOf  = return ... index
    nowIndexIn  = return ... inRange
    getOffsetOf = return ... offset
    
    getEitherViewOf = pure ... eitherViewOf

instance Monad m => BorderedM m [e] Int
  where
    getLower  _ = return 0
    getUpper es = return (length es - 1)
    
    getEitherViewOf = pure ... eitherViewOf




