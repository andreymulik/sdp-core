{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE Safe, CPP, TypeFamilies #-}

{- |
    Module      :  SDP.Concat
    Copyright   :  (c) Andrey Mulik 2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Concat" provides 'Concat' and 'ConcatM' classes.
-}
module SDP.Concat
(
  -- * Exports
  module SDP.Nullable,
  
  -- * Concat class
  Concat (..), Buffer (..), (++),
  
  -- * ConcatM class
  ConcatM (..), BufferM (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Nullable

import qualified Data.List as L
import Data.Kind

default ()

infixr 5 ++

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  A helper class for concatenating structures. Changing definitions of 'Concat'
  are optional and are needed only in cases where the standard operations are
  inefficient.
  
  @
    mempty === lzero
    foldMap === concatMap
    (<>) === (++) === mappend
    concat === fold === mconcat
  @
-}
class
    (
#if !MIN_VERSION_base(4,11,0)
      Nullable l, Semigroup l, Monoid l
#else
      Nullable l, Monoid l
#endif
    ) => Concat l
  where
    concat :: Foldable f => f l -> l
    concat =  fold
    
    concatMap :: Foldable f => (a -> l) -> f a -> l
    concatMap =  foldMap

--------------------------------------------------------------------------------

-- | Same as @('<>')@, concatenation of two structures.
(++) :: Concat l => l -> l -> l
(++) =  (<>)

--------------------------------------------------------------------------------

class (Concat l, NullableM m (BufferFor m l)) => Buffer m l | l -> m
  where
    type BufferFor m l :: Type
    
    unsafeFromBuffer :: BufferFor m l -> m l
    
    fromBuffer :: BufferFor m l -> m l
    
    appendBuffer :: BufferFor m l -> l -> m ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Monadic version of 'Concat'.
-}
class (Monad m, NullableM m l) => ConcatM m l | l -> m
  where
    {-# MINIMAL ((<~>)|concatM) #-}
    
    (<~>) :: l -> l -> m l
    xs <~> ys = concatM [xs, ys]
    
    concatM :: Foldable f => f l -> m l
    concatM =  foldr (\ x xs' -> do xs <- xs'; x <~> xs) newNull
    
    concatMapM :: Foldable f => (a -> m l) -> f a -> m l
    concatMapM go = foldr (\ a xs' -> do x <- go a; xs <- xs'; x <~> xs) newNull

--------------------------------------------------------------------------------

instance Concat [e]
  where
    concat    = L.concat
    concatMap = L.concatMap

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Create resizable buffer for mutable structure.
-}
class (ConcatM m l, NullableM m (BufferForM m l)) => BufferM m l
  where
    -- | Buffer type
    type BufferForM m l :: Type
    
    -- | Create a structure from given buffer, may reuse the buffer itself.
    unsafeFromBufferM :: BufferForM m l -> m l
    
    -- | Create a structure from given buffer, can't reuse the buffer itself.
    fromBufferM :: BufferForM m l -> m l
    
    -- | Append content of structure to existing buffer
    appendBufferM :: BufferForM m l -> l -> m ()



