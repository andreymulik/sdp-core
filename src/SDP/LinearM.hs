{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, DefaultSignatures, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE Safe, CPP, BangPatterns, GADTs, ViewPatterns, PatternSynonyms #-}

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{- |
    Module      :  SDP.LinearM
    Copyright   :  (c) Andrey Mulik 2019-2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.LinearM" is a module that provides 'BorderedM' and 'LinearM' classes.
-}
module SDP.LinearM
(
  -- * Exports
  module SDP.SequenceM,
  module SDP.Linear,
  module SDP.Concat,
  
  -- * LinearM class
  LinearM (..), LinearM1, LinearM2, unconsM, unsnocM, headM, tailM, initM, lastM,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  LinearM', LinearM'',
#endif
  
  -- * Extra functions
  intersperseM, intercalateM, eachM, eachFromM, mreplicate, iterateM,
  unsafeSwapM,
  
  -- ** Splits
  saveM, skipM, partsM, chunksM,
  
  -- ** Conditional splits
  spanlM, breaklM, splitByM, spanrM, breakrM, divideByM, dropWhileEndM,
  
  -- ** Filters
  exceptM, partitionM, partitionsM,
  
  -- ** Scans
  tailsM, initsM, splitsM, dividesM
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.SequenceM
import SDP.Linear
import SDP.Concat

import Control.Exception.SDP

default ()

infixl 5 !*

--------------------------------------------------------------------------------

{- |
  'LinearM' is 'Linear' version for mutable data structures. This class is
  designed with the possibility of in-place implementation, so many operations
  from 'Linear' have no analogues here.
-}
class (EstimateM m l, SequenceM m l e, ForceableM m l, ConcatM m l)
    => LinearM m l e | l -> m, l -> e
  where
    {-# MINIMAL ((headM',tailM')|unsnocM'), (takeM|sansM),
                ((initM',lastM')|unconsM'), (dropM|keepM),
                (!*), writeM, unsafeCopyTo,
                (newLinear|fromFoldableM) #-}
    
    {- Item-level operations. -}
    
    {- |
      Prepends new element to the start of the structure (monadic 'toHead').
      Like most size-changing operations, @prepend@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    prepend :: e -> l -> m l
    prepend e es = do e' <- singleM e; e' <~> es
    
    {- |
      Appends new element to the end of the structure (monadic 'toLast').
      Like most size-changing operations, @append@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    append :: l -> e -> m l
    append es e = do e' <- singleM e; es <~> e'
    
    {- |
      @since 0.3
      
      Separates mutable structure to head and tail.
    -}
    default unconsM' :: l -> m (Maybe (e, l))
    unconsM' :: l -> m (Maybe (e, l))
    unconsM' es = liftA2 (liftA2 (,)) (headM' es) (tailM' es)
    
    {- |
      @since 0.3
      
      Separates mutable structure to init and last.
    -}
    default unsnocM' :: l -> m (Maybe (l, e))
    unsnocM' :: l -> m (Maybe (l, e))
    unsnocM' es = liftA2 (liftA2 (,)) (initM' es) (lastM' es)
    
    {- |
      @since 0.3
      
      Returns head element of line or 'Nothing'.
    -}
    headM' :: l -> m (Maybe e)
    headM' =  fmap fsts . unconsM'
    
    {- |
      @since 0.3
      
      Returns tail of line or 'Nothing'.
    -}
    tailM' :: l -> m (Maybe l)
    tailM' =  fmap snds . unconsM'
    
    {- |
      @since 0.3
      
      Returns init of line or 'Nothing'.
    -}
    initM' :: l -> m (Maybe l)
    initM' =  fmap fsts . unsnocM'
    
    {- |
      @since 0.3
      
      Returns last element of line or 'Nothing'.
    -}
    lastM' :: l -> m (Maybe e)
    lastM' =  fmap snds . unsnocM'
    
    {- Splits. -}
    
    {- |
      @takeM n es@ returns a reference to the @es@, keeping first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    takeM :: Int -> l -> m l
    takeM n es = do s <- getSizeOf es; sansM (s - n) es
    
    {- |
      @dropM n es@ returns a reference to the @es@, discarding first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    dropM :: Int -> l -> m l
    dropM n es = do s <- getSizeOf es; keepM (s - n) es
    
    {- |
      @keepM n es@ returns a reference to the @es@, keeping last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    keepM :: Int -> l -> m l
    keepM n es = do s <- getSizeOf es; dropM (s - n) es
    
    {- |
      @sansM n es@ returns a reference to the @es@, discarding last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    sansM :: Int -> l -> m l
    sansM n es = do s <- getSizeOf es; takeM (s - n) es
    
    {- |
      @splitM n es@ returns pair of references to the @es@: keeping and
      discarding first @n@ elements. Changes in the source and result must be
      synchronous.
    -}
    splitM :: Int -> l -> m (l, l)
    splitM n es = liftA2 (,) (takeM n es) (dropM n es)
    
    {- |
      @divideM n es@ returns pair of references to the @es@: discarding and
      keeping last @n@ elements. Changes in the source and results must be
      synchronous.
    -}
    divideM :: Int -> l -> m (l, l)
    divideM n es = liftA2 (,) (sansM n es) (keepM n es)
    
    {- Item(s) from/to sequence. -}
    
    -- | Monadic 'single'.
    singleM :: e -> m l
    singleM =  newLinear . single
    
    -- | Monadic 'fromList'.
    {-# INLINE newLinear #-}
    newLinear :: [e] -> m l
    newLinear =  fromFoldableM
    
    -- | Monadic 'fromListN'.
    {-# INLINE newLinearN #-}
    newLinearN :: Int -> [e] -> m l
    newLinearN =  newLinear ... take
    
    -- | Monadic 'fromFoldable'.
    {-# INLINE fromFoldableM #-}
    fromFoldableM :: Foldable f => f e -> m l
    fromFoldableM =  newLinear . toList
    
    {- |
      @since 0.3
      
      Monadic version of 'replicate'.
    -}
    {-# INLINE replicateM #-}
    replicateM :: Int -> m e -> m l
    replicateM n e = newLinearN n =<< sequence (replicate n e)
    
    {- |
      @since 0.3
      
      Monadic 'reverseM', returns new structure.
    -}
    {-# INLINE reversed #-}
    reverseM :: l -> m l
    reverseM =  newLinear <=< getRight
    
    {- |
      @since 0.3
      
      Monadic in-place 'reverse', reverse elements of given structure.
    -}
    default reversed :: l -> m ()
    reversed :: l -> m ()
    reversed es = do
      n <- getSizeOf es
      forM_ [0 .. n `quot` 2 - 1] $ \ i ->
        unsafeSwapM es i (n - 1 - i)
    
    {- Filtering operations. -}
    
    filterM :: (e -> m Bool) -> l -> m l
    filterM go = newLinear <=< foldrM (\ e xs -> go e ?^ pure (e : xs) $ pure xs) []
    
    {- Conditional splits. -}
    
    -- | Splits line by separation elements.
    splitsByM :: (e -> Bool) -> l -> m [l]
    splitsByM f = mapM newLinear . splitsBy f <=< getLeft
    
    -- | Takes the longest 'prefixM' by predicate.
    takeWhileM :: (e -> Bool) -> l -> m l
    takeWhileM p es = do n <- prefixM p es; takeM n es
    
    -- | Takes the longest 'suffixM' by predicate.
    takeEndM :: (e -> Bool) -> l -> m l
    takeEndM p es = do n <- suffixM p es; keepM n es
    
    -- | Drops the longest 'prefixM' by predicate.
    dropWhileM :: (e -> Bool) -> l -> m l
    dropWhileM p es = do n <- prefixM p es; dropM n es
    
    -- | Drops the longest 'suffixM' by predicate.
    dropEndM :: (e -> Bool) -> l -> m l
    dropEndM p es = do n <- suffixM p es; sansM n es
    
    {- |
      @since 0.3
      
      @splitsOnM sub line@ splits @line@ by @sub@. See 'splitsOn'.
    -}
    splitsOnM :: Eq e => l -> l -> m [l]
    splitsOnM sub line = do
      s  <- getSizeOf sub
      is <- infixesM  sub line
      ps <- partsM    is  line
      forM ps (dropM s)
    
    {- |
      @since 0.3
      
      @infixesM inf es@ returns a list of @inf@ positions in @es@, without
      intersections. See 'infixes'.
    -}
    infixesM :: Eq e => l -> l -> m [Int]
    infixesM =  liftA2 infixes `on` getLeft
    
    {- |
      @since 0.3
      
      The @isSubseqMOf sub line@ checks if all the elements of the @susb@ occur,
      in order, in the @line@. The elements don't have to occur consecutively.
      See 'isSubseqOf'.
    -}
    isSubseqMOf :: Eq e => l -> l -> m Bool
    isSubseqMOf =  liftA2 isSubseqOf `on` getLeft
    
    {- |
      @since 0.3
      
      @isPrefixMOf sub line@ checks if @sub@ is beginning of @line@.
      See 'isPrefixOf'.
    -}
    isPrefixMOf :: Eq e => l -> l -> m Bool
    isPrefixMOf =  liftA2 isPrefixOf `on` getLeft
    
    {- |
      @since 0.3
      
      @isSuffixMOf sub line@ checks if @sub@ is ending of @line@.
      See 'isSuffixOf'.
    -}
    isSuffixMOf :: Eq e => l -> l -> m Bool
    isSuffixMOf =  liftA2 isSuffixOf `on` getLeft
    
    {- |
      @since 0.3
      
      @isInfixMOf sub line@ checks if @sub@ is substring of @line@.
      See 'isInfixOf'.
    -}
    isInfixMOf :: Eq e => l -> l -> m Bool
    isInfixMOf =  liftA2 isInfixOf `on` getLeft
    
    {- Pad/remove/replace. -}
    
    {- |
      @replaceByM sub new line@ replace every non-overlapping occurrence of
      @sub@ in @line@ with @new@.
      
      > replaceByM "foo" "bar" "foobafoorbaz" == "barbabarrbaz"
    -}
    replaceByM :: Eq e => l -> l -> l -> m l
    replaceByM sub new = intercalateM new <=< splitsOnM sub
    
    {- |
      @removeAllM sub line@ removes every non-overlapping occurrence of @sub@ in
      @line@.
    -}
    removeAllM :: Eq e => l -> l -> m l
    removeAllM =  concatM <=<< splitsOnM
    
    {- Operations with elements. -}
    
    {- |
      @since 0.3
      
      Monadic offset-based reader.
    -}
    (!*) :: MonadFail m => l -> Int -> m e
    
    {- |
      @since 0.3
      
      Unsafe monadic offset-based reader.
    -}
    default unsafeReadByOff :: MonadFail m => l -> Int -> m e
    unsafeReadByOff :: l -> Int -> m e
    unsafeReadByOff =  (!*)
    
    -- | Monadic offset-based writer.
    writeM :: MonadFail m => l -> Int -> e -> m ()
    
    {- |
      @since 0.3
      
      Unsafe monadic offset-based writer.
    -}
    default unsafeWriteM :: MonadFail m => l -> Int -> e -> m ()
    unsafeWriteM :: l -> Int -> e -> m ()
    unsafeWriteM =  writeM
    
    {-# INLINE unsafeCopyM #-}
    -- | @unsafeCopyM es l n@ returns the slice of @es@ from @l@ of length @n@.
    unsafeCopyM :: l -> Int -> Int -> m l
    unsafeCopyM es l n = do
      copy <- mreplicate n (emptyEx "unsafeCopyM")
      copy <$ unsafeCopyTo es l copy 0 n
    
    -- | @'removed' n es@ removes element with offset @n@ from @es@.
    removeM :: Int -> l -> m l
    removeM n es = newLinear . remove n =<< getLeft es
    
    {- |
      @since 0.3
      
      @'shiftM' es i j@ cyclically shifts the elements with offsets between
      @i@ and @j@ @(i < j)@ one position to the left (the @j@-th element is in
      the @i@-th position, the @i@-th in the @(i+1)@th, etc.) If @i >= j@,
      does nothing.
    -}
    shiftM :: l -> Int -> Int -> m ()
    shiftM es i j =
      let go k ej = when (k <= j) $ do
            ek <- unsafeReadByOff es k
            unsafeWriteM es k ej
            go (k + 1) ek
      in  when (i < j) $ go i =<< unsafeReadByOff es j
    
    {- |
      @unsafeCopyTo source soff target toff count@ writes @count@ elements of
      @source@ from @soff@ to @target@ starting with @toff@.
    -}
    unsafeCopyTo :: l -> Int -> l -> Int -> Int -> m ()
    unsafeCopyTo xs ox ys oy n = forM_ [0 .. n] $ \ i -> do
      x <- unsafeReadByOff xs (ox + i)
      unsafeWriteM ys (oy + i) x

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'unconsM' returns head and tail of structure. Fails if structure is empty.
-}
unconsM :: (MonadFail m, LinearM m l e) => l -> m (e, l)
unconsM es = do Just (h, t) <- unconsM' es; return (h, t)

{- |
  @since 0.3
  
  'unsnocM' returns init and last of structure. Fails if structure is empty.
-}
unsnocM :: (MonadFail m, LinearM m l e) => l -> m (l, e)
unsnocM es = do Just (i, l) <- unsnocM' es; return (i, l)

-- | 'headM' returns head of structure. Fails if structure is empty.
headM :: (MonadFail m, LinearM m l e) => l -> m e
headM es = do Just x <- headM' es; return x

{- |
  @since 0.3
  
  'tailM' returns tail of structure. Fails if structure is empty.
-}
tailM :: (MonadFail m, LinearM m l e) => l -> m l
tailM es = do Just xs <- tailM' es; return xs

{- |
  @since 0.3
  
  'initM' returns init of structure. Fails if structure is empty.
-}
initM :: (MonadFail m, LinearM m l e) => l -> m l
initM es = do Just xs <- initM' es; return xs

-- | 'lastM' returns last element of structure. Fails if structure is empty.
lastM :: (MonadFail m, LinearM m l e) => l -> m e
lastM es = do Just x <- lastM' es; return x

--------------------------------------------------------------------------------

-- | 'LinearM' contraint for @(Type -> Type)@-kind types.
type LinearM1 m l e = LinearM m (l e) e

-- | 'LinearM' contraint for @(Type -> Type -> Type)@-kind types.
type LinearM2 m l i e = LinearM m (l i e) e

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'LinearM' contraint for @(Type -> Type)@-kind types.
type LinearM' m l = forall e . LinearM m (l e) e

-- | 'LinearM' contraint for @(Type -> Type -> Type)@-kind types.
type LinearM'' m l = forall i e . LinearM m (l i e) e
#endif

--------------------------------------------------------------------------------

{- Extra splits. -}

{- |
  @saveM n es@ takes first @n@ elements of @es@ if @n > 0@ and last @-n@
  elements otherwise.
-}
saveM :: LinearM m l e => Int -> l -> m l
saveM n = n > 0 ? takeM n $ keepM (-n)

{- |
  @skipM n es@ drops first @n@ elements of @es@ if @n > 0@ and last @-n@
  elements otherwise.
-}
skipM :: LinearM m l e => Int -> l -> m l
skipM n = n > 0 ? dropM n $ sansM (-n)

{- |
  @partsM n es@ returns the sequence of @es@ prefix references, splitted by
  offsets in @es@. Changes in the source and results must be synchronous.
-}
partsM :: (Foldable f, LinearM m l e) => f Int -> l -> m [l]
partsM =  splitsM . go . toList where go is = zipWith (-) is (0 : is)

{- |
  @chunksM n es@ returns the sequence of @es@ prefix references of length
  @n@. Changes in the source and results must be synchronous.
-}
chunksM :: LinearM m l e => Int -> l -> m [l]
chunksM n es = do (t, d) <- splitM n es; isNullM d ?^ pure [t] $ (t :) <$> chunksM n d

--------------------------------------------------------------------------------

{- Extra conditional splits. -}

-- | Left-side span.
spanlM :: LinearM m l e => (e -> Bool) -> l -> m (l, l)
spanlM p es = liftA2 (,) (takeWhileM p es) (dropWhileM p es)

-- | Left-side break.
breaklM :: LinearM m l e => (e -> Bool) -> l -> m (l, l)
breaklM =  spanlM . (not .)

{- |
  @splitByM p es@ split line by first (left) found separation element. See 'splitBy'.
  
  @
    splitByM (== '.') "foo" == ("foo","")
    splitByM (== '.') "foo." == ("foo","")
    splitByM (== '.') ".foo" == ("","foo")
    splitByM (== '.') "foo.bar" == ("foo","bar")
    splitByM (== '.') "foo.bar.baz" == ("foo","bar.baz")
  @
-}
splitByM :: LinearM m l e => (e -> Bool) -> l -> m (l, l)
splitByM p es = do (t, d') <- breaklM p es; d <- dropM 1 d'; return (t, d)

-- | Right-side span.
spanrM :: LinearM m l e => (e -> Bool) -> l -> m (l, l)
spanrM p es = liftA2 (,) (dropEndM p es) (takeEndM p es)

-- | Right-side break.
breakrM :: LinearM m l e => (e -> Bool) -> l -> m (l, l)
breakrM =  spanrM . (not .)

{- |
  @divideByM p es@ split line by last (right) found separation element. See 'divideBy'.
  
  > divideByM (== '.') "foo" == ("","foo")
  > divideByM (== '.') ".foo" == ("","foo")
  > divideByM (== '.') "foo." == ("foo","")
  > divideByM (== '.') "foo.bar" == ("foo","bar")
  > divideByM (== '.') "foo.bar.baz" == ("foo.bar","baz")
-}
divideByM :: LinearM m l e => (e -> Bool) -> l -> m (l, l)
divideByM p es = -- first (sans 1) ... breakr
  do (t', d) <- breakrM p es; t <- sansM 1 t'; return (t, d)

{- |
  @since 0.3
  
  @dropWhileEndM f = dropWhile f <=< dropEnd f@.
-}
dropWhileEndM :: LinearM m l e => (e -> Bool) -> l -> m l
dropWhileEndM f = dropWhileM f <=< dropEndM f

--------------------------------------------------------------------------------

{- Extra filters. -}

exceptM :: LinearM m l e => (e -> m Bool) -> l -> m l
exceptM p = filterM (\ x -> not <$> p x)

partitionM :: LinearM m l e => (e -> m Bool) -> l -> m (l, l)
partitionM p es = liftA2 (,) (filterM p es) (exceptM p es)

-- | Generalization of partition, that select sublines by predicates.
partitionsM :: (LinearM m l e, Foldable f) => f (e -> m Bool) -> l -> m [l]
partitionsM =  partitionsM_ . toList
  where
    partitionsM_    []    es = return [es]
    partitionsM_ (p : ps) es = do
      (f, e) <- partitionM  p es
      fs     <- partitionsM ps e
      return (f : fs)

--------------------------------------------------------------------------------

{- Extra scans. -}

-- | 'tailsM' returns sequence of @es@ 'getTail'.
tailsM :: LinearM m l e => l -> m [l]
tailsM es = do
  decon <- unconsM' es
  case decon of
    Just (_, ts) -> (es :) <$> tailsM ts
    Nothing      -> single <$> newNull

-- | 'inits' returns sequence of @es@  'init'.
initsM :: LinearM m l e => l -> m [l]
initsM =  fmap reverse . initsM_
  where
    initsM_ es = do
      decon <- unsnocM' es
      case decon of
        Just (is, _) -> (es :) <$> initsM_ is
        Nothing      -> single <$> newNull

{- |
  @splitM ns es@ returns the sequence of @es@ prefix references of length
  @n <- ns@. Changes in the source and results must be synchronous.
-}
splitsM :: LinearM m l e => [Int] -> l -> m [l]
splitsM    []    es = return [es]
splitsM (n : ns) es = do
  (x, xs') <- splitM   n es
  xs       <- splitsM ns xs'
  return (x : xs)

{- |
  @dividesM ns es@ returns the sequence of @es@ suffix references of length
  @n <- ns@. Changes in the source and results must be synchronous.
-}
dividesM :: LinearM m l e => [Int] -> l -> m [l]
dividesM =  \ ns es -> reverse <$> dividesM_ ns es
  where
    dividesM_    []    es = return [es]
    dividesM_ (n : ns) es = do
      (xs', x) <- divideM    n es
      xs       <- dividesM_ ns xs'
      return (x : xs)

--------------------------------------------------------------------------------

-- | Generalized 'intersperse'.
intersperseM :: LinearM m l e => e -> l -> m l
intersperseM e es = isNullM es ?^ return es $ do
  xs <- foldrM (\ x xs -> return (e : x : xs)) [] es
  newLinear (drop 1 xs)

-- | intercalate is generalization of intercalate
intercalateM :: (Foldable f, LinearM m l e) => l -> f l -> m l
intercalateM es = concatM . intersperse es . toList

{- |
  @eachM n es@ returns new sequence of @es@ elements with step @n@. eachM
  shouldn't return references to @es@.
-}
eachM :: LinearM m l e => Int -> l -> m l
eachM n es = case n <=> 1 of
  GT -> newLinear =<< ofoldrM (\ i x xs -> pure $ mod i n == 0 ? x : xs $ xs) [] es
  EQ -> return es
  LT -> newNull

eachFromM :: LinearM m l e => Int -> Int -> l -> m l
eachFromM o n = eachM n <=< dropM o

{- |
  @since 0.3
  
  Mutable version of 'iterate'.
-}
iterateM :: LinearM m l e => Int -> (e -> m e) -> e -> m l
iterateM n go e = newLinearN n =<< iterate' n e id
  where
    iterate' 0 _ xs = return (xs [])
    iterate' i x xs = do x' <- go x; iterate' (i - 1) x' (xs . (x :))

{- |
  @since 0.3
  
  Monadic version of 'replicate'.
-}
mreplicate :: LinearM m l e => Int -> e -> m l
mreplicate n e = newLinearN n (replicate n e)

{- |
  @since 0.3
  
  Just swap two elements.
-}
unsafeSwapM :: LinearM m l e => l -> Int -> Int -> m ()
unsafeSwapM es i j = do
  ei <- unsafeReadByOff es i
  ej <- unsafeReadByOff es j
  unsafeWriteM es i ej
  unsafeWriteM es j ei

--------------------------------------------------------------------------------

emptyEx :: String -> a
emptyEx =  throw . PatternMatchFail . showString "in SDP.LinearM."


