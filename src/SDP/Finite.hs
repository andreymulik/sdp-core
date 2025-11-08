{-# LANGUAGE Trustworthy, UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DeriveDataTypeable, DeriveGeneric #-}

{- |
    Module      :  SDP.Finite
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Finite" provide generalized finite-dimensional index type (':&') based
    on @repa@ @(:.)@.
    
    Since @sdp-0.2@, for (':&') available @OverloadedLists@-based syntactic sugar.
    For example, instead of the inconvenient @es!(ind4 0 1 2 3)@ or just awful
    @es!(E:&0:&1:&1:&2:&3)@ you can write: @es![0, 1, 2, 3]@.
    
    Note that @OverloadedLists@ instances requires a strictly defined number of
    subindexes.
-}
module SDP.Finite
(
  -- * Generalized index
  E (..), (:&) (..),
  
  -- * Type synonyms
  I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15
)
where

import SDP.Nullable

import Data.Bifunctor
import Data.Data

import GHC.Generics
import GHC.Types
import GHC.Read

import qualified GHC.Exts as E
import GHC.Exts ( IsList )

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- Zero-dimensional type. -}

-- | Service type, that represents zero-dimensional index.
data E = E deriving ( Eq, Ord, Show, Read, Data, Generic )

instance IsList E
  where
    type Item E = E
    
    fromList = const E
    toList   = const []

-- | @since 0.2.1
instance Nullable E where lzero = E; isNull = const True

--------------------------------------------------------------------------------

{- N-dimensional index type. -}

{- |
  N-dimensional index type. The type (head :& tail) allows working with any
  finite dimension number.
-}
data tail :& head = !tail :& !head deriving ( Eq, Ord, Data, Generic )

instance Enum i => Enum (E :& i)
  where
    fromEnum (E :& e) = fromEnum  e
    succ     (E :& e) = E :& succ e
    pred     (E :& e) = E :& pred e
    
    toEnum = (E :&) . toEnum
    
    enumFrom                (E :& f)          = (E :&) <$> [f ..]
    enumFromTo         (E :& f) (E :& l)      = (E :&) <$> [f .. l]
    enumFromThen       (E :& f)  (E :& n)     = (E :&) <$> [f, n ..]
    enumFromThenTo (E :& f) (E :& n) (E :& l) = (E :&) <$> [f, n .. l]

--------------------------------------------------------------------------------

{- Overloaded indices. -}

instance (IsList (i' :& i), E.Item (i' :& i) ~~ i, Show i) => Show (i' :& i)
  where
    showsPrec p = showsPrec p . E.toList

instance (IsList (i' :& i), E.Item (i' :& i) ~~ i, Read i) => Read (i' :& i)
  where
    readPrec = E.fromList <$> readPrec

instance IsList (E :& i)
  where
    type Item (E :& i) = i
    
    fromList [i] = E :& i
    fromList  _  = throw $ UnexpectedRank "in SDP.Finite.fromList"
    
    toList (E :& i) = [i]

instance (E.Item (i' :& i) ~~ i, IsList (i' :& i)) => IsList (i' :& i :& i)
  where
    type Item (i' :& i :& i) = i
    
    toList (i' :& i) = E.toList i' ++ [i]
    
    fromList = (uncurry $ (:&) . E.fromList) . unsnoc

--------------------------------------------------------------------------------

{- Type synonyms are declared up to 15 dimensions. -}

-- | 1-dimensional index (@(E :& i)@ without @TypeOperators@).
type I1  i = E :& i
-- | 2-dimensional index
type I2  i = E :& i  :& i
-- | 3-dimensional index
type I3  i = (I2  i) :& i
-- | 4-dimensional index
type I4  i = (I3  i) :& i
-- | 5-dimensional index
type I5  i = (I4  i) :& i
-- | 6-dimensional index
type I6  i = (I5  i) :& i
-- | 7-dimensional index
type I7  i = (I6  i) :& i
-- | 8-dimensional index
type I8  i = (I7  i) :& i
-- | 9-dimensional index
type I9  i = (I8  i) :& i
-- | 10-dimensional index
type I10 i = (I9  i) :& i
-- | 11-dimensional index
type I11 i = (I10 i) :& i
-- | 12-dimensional index
type I12 i = (I11 i) :& i
-- | 13-dimensional index
type I13 i = (I12 i) :& i
-- | 14-dimensional index
type I14 i = (I13 i) :& i
-- | 15-dimensional index
type I15 i = (I14 i) :& i

--------------------------------------------------------------------------------

unsnoc :: [i] -> ([i], i)
unsnoc    [i]   = ([], i)
unsnoc (i : is) = (i :) `first` unsnoc is
unsnoc     _    = throw $ UnexpectedRank "in SDP.Finite.fromList"




