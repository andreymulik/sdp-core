{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE Trustworthy, TypeFamilies, ConstraintKinds #-}

{- |
    Module      :  SDP.Var
    Copyright   :  (c) Andrey Mulik 2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Var" provides variable types and classes.
    
    @since 0.3
-}
module SDP.Var
(
  IsMVar (..), Var, MonadVar,
  
  STRef, STRef', IORef, IORef', AtomicIORef, MVar, TVar
)
where

import Data.Coerce
import Data.IORef
import Data.STRef
import Data.Kind

import GHC.Conc

import Control.Monad.ST
import Control.Concurrent.MVar

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @'MonadVar' m@ is synonym for @'IsMVar' m ('Var' m)@
-}
type MonadVar m = (Monad m, IsMVar m (Var m))

{- |
  @since 0.3
  
  'Var' is type family of basic variables for each monad.
-}
type family Var (m :: Type -> Type) :: Type -> Type

type instance Var (ST s) = STRef s
type instance Var IO     = IORef
type instance Var STM    = TVar

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Class of mutable variables.
-}
class Monad m => IsMVar m var | var -> m
  where
    {-# MINIMAL newVar, readVar, writeVar #-}
    
    {- |
      @since 0.3
      
      'newVar' creates new mutable variable with given value.
    -}
    newVar :: e -> m (var e)
    
    {- |
      @since 0.3
      
      'readVar' returns current variable value.
    -}
    readVar :: var e -> m e
    
    {- |
      @since 0.3
      
      'writeVar' writes given value to variable.
    -}
    writeVar :: var e -> e -> m ()
    
    {- |
      @since 0.3
      
      'updateVar' updates current value of variable by given function.
    -}
    updateVar :: var e -> (e -> e) -> m ()
    updateVar var f = writeVar var . f =<< readVar var

--------------------------------------------------------------------------------

instance IsMVar (ST s) (STRef s)
  where
    newVar    = newSTRef
    readVar   = readSTRef
    writeVar  = writeSTRef
    updateVar = modifySTRef

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'STRef'' is version of 'STRef' with strict 'updateVar'. See "Data.STRef" for
  more details.
-}
newtype STRef' s a = STRef' (STRef s a)
  deriving ( Eq )

instance IsMVar (ST s) (STRef' s)
  where
    newVar    = coerce . newSTRef
    readVar   = readSTRef . coerce
    writeVar  = writeSTRef . coerce
    updateVar = modifySTRef' . coerce

--------------------------------------------------------------------------------

instance IsMVar IO IORef
  where
    newVar    = newIORef
    readVar   = readIORef
    writeVar  = writeIORef
    updateVar = modifyIORef

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'IORef'' is version of 'IORef' with strict 'updateVar'.
-}
newtype IORef' a = IORef' (IORef a)
  deriving ( Eq )

instance IsMVar IO IORef'
  where
    newVar    = coerce . newIORef
    readVar   = readIORef . coerce
    writeVar  = writeIORef . coerce
    updateVar = modifyIORef' . coerce

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'IORef'' is version of 'IORef' with strict and atomic 'writeVar' and
  'updateVar'.
-}
newtype AtomicIORef a = AtomicIORef (IORef a)
  deriving ( Eq )

instance IsMVar IO AtomicIORef
  where
    newVar    = coerce . newIORef
    readVar   = readIORef . coerce
    writeVar  = atomicWriteIORef . coerce
    updateVar = \ var f -> atomicModifyIORef' (coerce var) (\ x -> (f x, ()))

--------------------------------------------------------------------------------

instance IsMVar IO MVar
  where
    newVar    = newMVar
    readVar   = readMVar
    writeVar  = \ var e -> () <$ swapMVar var e
    updateVar = \ var f -> modifyMVar_ var (return . f)

--------------------------------------------------------------------------------

instance IsMVar STM TVar
  where
    newVar   = newTVar
    readVar  = readTVar
    writeVar = writeTVar




