{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, CPP, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, BangPatterns #-}

#ifdef SDP_LINEAR_EXTRAS
{-# LANGUAGE FlexibleContexts, TypeOperators #-}
#endif

#ifdef SDP_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Linear
    Copyright   :  (c) Andrey Mulik 2019-2025
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Linear" is a module that provides several convenient interfaces for
    working with various linear data structures.
-}
module SDP.Linear
(
  -- * Exports
  module SDP.Forceable,
  module SDP.Nullable,
  module SDP.Sequence,
  module SDP.Concat,
  module SDP.Index,
  module SDP.Sort,
  module SDP.Zip,
  
  -- * Bordered class
  module SDP.Bordered,
  
  -- * Linear class
  Linear (..), Linear1, Linear2, pattern (:>), pattern (:<), uncons, unsnoc,
  
#ifdef SDP_QUALIFIED_CONSTRAINTS
  -- ** Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Linear', Linear'',
#endif
  
  -- * Extra functions
  intersperse, intercalate, subsequences, each, eachFrom, ascending,
  iterate,
  
  -- ** Folds
  sfoldr1, sfoldl1, sfoldr1', sfoldl1', csfoldr', csfoldl', msfoldr, msfoldl,
  
  -- ** Splits
  save, skip, parts, chunks,
  
  -- ** Conditional splits
  spanl, breakl, splitBy, spanr, breakr, divideBy, dropWhileEnd,
  
  -- ** Suffixes and prefixes
  stripPrefix, stripSuffix, stripPrefix', stripSuffix',
  
  -- ** Filters
  except, partition, partitions,
  
  -- *** Monadic filters
  mexcept, mfilter, mpartition,
  
  -- ** Scans
  tails, inits, splits, divides,
  
  -- ** Selects
  select, select', extract, extract',
  
  -- *** Conditional selections
  selectWhile, selectWhile', selectEnd, selectEnd',
  extractWhile, extractWhile', extractEnd, extractEnd',
  
  -- ** Monadic selects
  mselect, mselect', mextract, mextract'
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Forceable
import SDP.Nullable
import SDP.Sequence
import SDP.Bordered
import SDP.Concat
import SDP.Index
import SDP.Sort
import SDP.Zip

import qualified Data.List as L

#ifdef SDP_LINEAR_EXTRAS
import qualified GHC.Exts as L
#endif

import Control.Exception.SDP

default ()

infix  8 `filter`, `except`
infixr 5 :>
infixl 5 :<
infixl 9 !!

--------------------------------------------------------------------------------

{-# WARNING uncons "This is a partial function, it throws an error on empty lists. Use pattern matching, 'Data.List.uncons' instead." #-}
{-# WARNING unsnoc "This is a partial function, it throws an error on empty lists. Use pattern matching, 'Data.List.unsnoc' instead." #-}
{-# WARNING head   "This is a partial function, it throws an error on empty lists. Use pattern matching, 'Data.List.uncons' instead." #-}
{-# WARNING tail   "This is a partial function, it throws an error on empty lists. Use pattern matching, 'Data.List.uncons' instead." #-}
{-# WARNING init   "This is a partial function, it throws an error on empty lists. Use pattern matching, 'Data.List.unsnoc' instead." #-}
{-# WARNING last   "This is a partial function, it throws an error on empty lists. Use pattern matching, 'Data.List.unsnoc' instead." #-}

{- |
  'Linear' is one of the main SDP classes, a class of linear data structures.
  
  A structure of type @l@ must be able to contain an arbitrary ordered dataset
  of type @e@, with arbitrary length. The ordering implies that each @e@ element
  in @l@ can be associated with a non-negative number.
  
  Structures that cannot store, for example, a zero number of elements (e.g.
  'Data.List.NonEmpty.NonEmpty'), or have some static size, aren't 'Linear'.
  
  Of course, the @l@ structure can also have a greater dimension, for example,
  a matrix (of scalar elements) can be 'Linear' if some order is defined for its
  elements. The standard SDP structures use the 'Index' class to define this
  order. You may also want to think of the table as a linear structure
  containing rows/columns, or use a different ordering method such as Gray code.
  
  'Linear' structures must follow some rules:
  
  @
    fromList === fromFoldable
    fromFoldable === sfoldr toHead Z
    
    isNull  (single e)   === False
    isNull (toHead x xs) === False
    isNull (toLast xs x) === False
    
    sfoldr  === ofoldr . const
    sfoldl  === ofoldl . const
    sfoldr1 === ofoldr1 . const
    sfoldl1 === ofoldl1 . const
    
    reverse . reverse === id
    listL === reverse . listR === listR . reverse === listR . listR
    listR === reverse . listL === listL . reverse
    
    ofoldr f base xs === sfoldr (uncurry f) base (assocs xs)
    filter p = fromList . sfoldr (\ x xs -> p x ? x : xs $ xs) []
    
    -- For ghc >= 8.4
    toList   = 'L.toList'
    fromList = 'L.fromList'
    
    -- For 'Foldable' instances:
    toList === listL
    length === sizeOf
    length (replicate n e) === n
    length (toHead x xs) === length xs + 1
    length (toLast xs x) === length xs + 1
    
    isNull xs === length xs == 0
    length xs === length (listL xs) === length (listR xs)
    
    sfoldr  === foldr
    sfoldl  === foldl
    sfoldr1 === foldr1
    sfoldl1 === foldl1
  @
  
  The 'L.IsItem' 'L.fromListN' and 'Linear' 'fromListN' functions may not be
  identical, since 'L.IsItem' @'L.fromListN' n es@ is not defined for the case
  @n =/= length es@.
  
  If @n >= length es@, then 'Linear' 'fromListN' must create a valid structure
  of @length es@.
  
  If @n < length es@, then 'Linear' 'fromListN' must truncate the list according
  like 'take' function does.
-}
class
  (
#ifdef SDP_LINEAR_EXTRAS
    L.IsList l, e ~ L.Item l,
#endif
    Sequence l e,
    Forceable l,
    Estimate l,
    Concat l
  ) => Linear l e | l -> e
  where
    {-# MINIMAL ((head,last)|uncons'), (take|sans), (toHead|single),
                ((init,last)|unsnoc'), (drop|keep) #-}
    
    {- Item-level operations. -}
    
    -- | Prepends element to line, constructor for ':>' pattern.
    toHead :: e -> l -> l
    toHead e es = single e ++ es
    
    -- | Appends element to line, constructor for ':<' pattern.
    toLast :: l -> e -> l
    toLast es e = es ++ single e
    
    -- | Same as @'isNull' '?-' 'uncons'@
    uncons' :: l -> Maybe (e, l)
    uncons' =  isNull ?- \ xs -> (head xs, tail xs)
    
    -- | Same as @'isNull' '?-' 'unsnoc'@
    unsnoc' :: l -> Maybe (l, e)
    unsnoc' =  isNull ?- \ xs -> (init xs, last xs)
    
    -- | Returns first element of line, may fail.
    head :: l -> e
    head =  fst . uncons
    
    -- | Returns line except first, may fail.
    tail :: l -> l
    tail =  snd . uncons
    
    -- | Returns line except 'last' element, may fail.
    init :: l -> l
    init =  fst . unsnoc
    
    -- | Returns last element, may fail.
    last :: l -> e
    last =  snd . unsnoc
    
    {- Splits. -}
    
    -- | @take n es@ takes first @n@ elements of @es@.
    take :: Int -> l -> l
    take n es = sans (sizeOf es - n) es
    
    -- | @drop n es@ drops first @n@ elements of @es@.
    drop :: Int -> l -> l
    drop n es = keep (sizeOf es - n) es
    
    -- | @keep n es@ takes last @n@ elements of @es@.
    keep :: Int -> l -> l
    keep n es = drop (sizeOf es - n) es
    
    -- | @sans n es@ drops last @n@ elements of @es@.
    sans :: Int -> l -> l
    sans n es = take (sizeOf es - n) es
    
    -- | @split n es@ is same to @(take n es, drop n es)@.
    split :: Int -> l -> (l, l)
    split n es = (take n es, drop n es)
    
    -- | @divide n es@ is same to @(sans n es, keep n es)@.
    divide :: Int -> l -> (l, l)
    divide n es = (sans n es, keep n es)
    
    {- Item(s) from/to sequence. -}
    
    -- | Just singleton.
    single :: e -> l
    single =  flip toHead Z
    
    {- |
      Creates line from list.
      
      Defaults:
      @
        fromList = Linear.fromFoldable
        
        -- For ghc >= 8.4, see SDP_LINEAR_EXTRAS
        fromList = IsList.fromList
      @
    -}
    fromList :: [e] -> l
#ifdef SDP_LINEAR_EXTRAS
    fromList =  L.fromList
#else
    fromList =  fromFoldable
#endif
    
    -- | Create finite line from (possibly infinite) list.
    fromListN :: Int -> [e] -> l
    fromListN =  fromList ... L.take
    
    -- | Generalized 'fromList'.
    fromFoldable :: Foldable f => f e -> l
    fromFoldable =  foldr toHead Z
    
    -- | @replicate n e@ returns a line of @n@ repetitions of the element @e@.
    replicate :: Int -> e -> l
    replicate n = fromListN n . replicate n
    
    -- | Returns sequence with reversed element order.
    reverse :: l -> l
    reverse =  fromList . listR
    
    {- |
      @since 0.3
      
      @unfoldr gen x@ creates new structure using producing function @gen@ and
      initial value @x@. @gen@ takes the element and returns 'Nothing' if it is
      done producing the linear structure or returns @Just (e, a)@, in which
      case, @e@ is a prepended to the structure and @a@ is used as the next
      element in a recursive call.
    -}
    unfoldr :: (b -> Maybe (e, b)) -> b -> l
    unfoldr =  fromList ... L.unfoldr
    
    {- Filtering operations. -}
    
    -- | Generalized filter.
    filter :: (e -> Bool) -> l -> l
    filter p = fromList . sfoldr (\ x xs -> p x ? x : xs $ xs) []
    
    {- Conditional splits. -}
    
    -- | Takes the longest 'prefix' by predicate.
    takeWhile :: (e -> Bool) -> l -> l
    takeWhile p es = take (prefix p es) es
    
    -- | Takes the longest 'suffix' by predicate.
    takeEnd :: (e -> Bool) -> l -> l
    takeEnd p es = keep (suffix p es) es
    
    -- | Drops the longest 'prefix' by predicate.
    dropWhile :: (e -> Bool) -> l -> l
    dropWhile p es = drop (prefix p es) es
    
    -- | Drops the longest 'suffix' by predicate.
    dropEnd :: (e -> Bool) -> l -> l
    dropEnd p es = sans (suffix p es) es
    
    -- | Splits line by separation elements.
    splitsBy :: (e -> Bool) -> l -> [l]
    splitsBy e = map fromList . splitsBy e . listL
    
    {- |
      @splitsOn sub line@ splits @line@ by @sub@.
      
      > splitsOn "fo" "foobar bazfoobar1" == ["","obar baz","obar1"]
    -}
    splitsOn :: Eq e => l -> l -> [l]
    splitsOn sub line = drop (sizeOf sub) <$> parts (infixes sub line) line
    
    {- Subsequence operations. -}
    
    {- |
      @infixes inf es@ returns a list of @inf@ positions in @es@, without
      intersections.
      
      > "" `infixes` es = []
      > "abba" `infixes` "baababba" == [4]
      > "abab" `infixes` "baababab" == [2]
      > "aaaa" `infixes` "aaaaaaaa" == [0, 4]
    -}
    infixes :: Eq e => l -> l -> [Int]
    infixes =  on infixes listL
    
    {- |
      The @isSubseqOf sub line@ checks if all the elements of @sub@ occur, in
      order, in the @line@. The elements don't have to occur consecutively.
    -}
    isSubseqOf :: Eq e => l -> l -> Bool
    isSubseqOf =  L.isSubsequenceOf `on` listL
    
    -- | @sub `'isPrefixOf'` line@ checks if @sub@ is beginning of @line@.
    isPrefixOf :: Eq e => l -> l -> Bool
    isPrefixOf =  isPrefixOf `on` listL
    
    -- | @sub `'isSuffixOf'` line@ checks if @sub@ is ending of @line@.
    isSuffixOf :: Eq e => l -> l -> Bool
    isSuffixOf =  isSuffixOf `on` listL
    
    -- | @sub `'isInfixOf'` line checks if @sub@ is substring of @line@.
    isInfixOf :: Eq e => l -> l -> Bool
    isInfixOf =  isInfixOf `on` listL
    
    {- Pad/remove/replace. -}
    
    {- |
      @since 0.3
      
      @pad n e es@ pads the @es@ line with repetitions of the element @e@ to
      length @n@. If the length of the original line is greater than @n@, then
      works like @take n@ or @keep n@ resp.
    -}
    pad :: Either Int Int -> e -> l -> l
    pad (Left  n) e es = keep n (es .> n ? es $ replicate n e ++ es)
    pad (Right n) e es = take n (es .> n ? es $ es ++ replicate n e)
    
    {- |
      @replaceBy sub new line@ replace every non-overlapping occurrence of @sub@
      in @line@ with @new@.
      
      > replaceBy "foo" "bar" "foobafoorbaz" == "barbabarrbaz"
    -}
    replaceBy :: Eq e => l -> l -> l -> l
    replaceBy sub new = intercalate new . splitsOn sub
    
    {- |
      Removes every non-overlapping occurrence of @sub@ with 'Z'.
      
      > removeAll = concat ... splitsOn
      > (`replaceBy` Z) = removeAll
    -}
    removeAll :: Eq e => l -> l -> l
    removeAll =  concat ... splitsOn
    
    {- Deduplication. -}
    
    -- | Same as @nubBy ('==')@.
    nub :: Eq e => l -> l
    nub =  nubBy (==)
    
    -- | Generalization of nubBy.
    nubBy :: Equal e -> l -> l
    nubBy f = fromList . nubBy f . listL
    
    {- Operations with elements. -}
    
    {- |
      Returns the element of a sequence by offset, may be completely unsafe.
      This is an optimistic read function and shouldn't perform checks for
      efficiency reasons.
      
      If you need safety, use (!) or (!?). The generalization of this function
      by index type (.!).
      
      > es !! i = listL es !! i
    -}
    (!!) :: l -> Int -> e
    (!!) =  (L.!!) . listL
    
    {- |
      @write es n e@ writes value @e@ in position @n@ (offset), returns new
      structure. If @n@ is out of range, returns equal structure (@es@ or copy).
    -}
    write :: l -> Int -> e -> l
    write es = fromList ... write (listL es)
    
    {- |
      @since 0.2.1
      
      @'remove' es i@ delete element with offset @i@ from @es@.
      
      > remove (-1) [0 .. 5] == [0 .. 5]
      > remove   6  [0 .. 5] == [0 .. 5]
      > remove   0  [0 .. 5] == [1,2,3,4,5]
      > remove   3  [0 .. 5] == [0,1,2,4,5]
      > remove   5  [0 .. 5] == [0,1,2,3,4]
    -}
    remove :: Int -> l -> l
    remove n = fromList . remove n . listL

--------------------------------------------------------------------------------

{- containers-style patterns. -}

-- | Pattern @(':>')@ is left-size view of line. Same as 'uncons' and 'toHead'.
pattern  (:>)   :: Linear l e => e -> l -> l
pattern x :> xs <- (uncons' -> Just (x, xs)) where (:>) = toHead

-- | Pattern @(':<')@ is right-size view of line. Same as 'unsnoc' and 'toLast'.
pattern   (:<)  :: Linear l e => l -> e -> l
pattern xs :< x <- (unsnoc' -> Just (xs, x)) where (:<) = toLast

--------------------------------------------------------------------------------

{- Trivial definitions, separated from the Linear class. -}

-- | Separates line to 'head' and 'tail', deconstructor for ':>' pattern.
uncons :: Linear l e => l -> (e, l)
uncons xs = case uncons' xs of {Just res -> res; _ -> pfailEx "(:>)"}

-- | Separates line to 'init' and 'last', deconstructor for ':<' pattern.
unsnoc :: Linear l e => l -> (l, e)
unsnoc xs = case unsnoc' xs of {Just res -> res; _ -> pfailEx "(:<)"}

{- |
  @iterate n f x@ returns sequence of @n@ applications of @f@ to @x@.
  
  Note that @iterate@ returns finite sequence, instead "Prelude" prototype.
-}
iterate :: Linear l e => Int -> (e -> e) -> e -> l
iterate n =
  let go c f e = c == 0 ? [] $ e : go (c - 1) f (f e)
  in  fromListN n ... go (max 0 n)

--------------------------------------------------------------------------------

{- Constraint types. -}

-- | 'Linear' contraint for @(Type -> Type)@-kind types.
type Linear1 l e = Linear (l e) e

-- | 'Linear' contraint for @(Type -> Type -> Type)@-kind types.
type Linear2 l i e = Linear (l i e) e

#ifdef SDP_QUALIFIED_CONSTRAINTS
-- | 'Linear' contraint for @(Type -> Type)@-kind types.
type Linear' l = forall e . Linear (l e) e

-- | 'Linear' contraint for @(Type -> Type -> Type)@-kind types.
type Linear'' l = forall i e . Linear2 l i e
#endif

--------------------------------------------------------------------------------

{- Extra folds. -}

{-# WARNING sfoldr1 "This is a partial function, it throws an error on empty lists." #-}

-- | 'sfoldr1' is just 'Data.Foldable.foldr1' in 'Linear' context.
sfoldr1 :: Linear l e => (e -> e -> e) -> l -> e
sfoldr1 f = \ es' -> case unsnoc' es' of
  Just (es, e) -> sfoldr f e es
  _            -> pfailEx "sfoldr1"

{-# WARNING sfoldl1 "This is a partial function, it throws an error on empty lists." #-}

-- | 'sfoldl1' is just 'Data.Foldable.foldl1' in 'Linear' context.
sfoldl1 :: Linear l e => (e -> e -> e) -> l -> e
sfoldl1 f = \ es' -> case uncons' es' of
  Just (e, es) -> sfoldl f e es
  _            -> pfailEx "sfoldl1"

{-# WARNING sfoldr1' "This is a partial function, it throws an error on empty lists." #-}

-- | 'sfoldr1'' is just strict 'Data.Foldable.foldr1' in 'Linear' context.
sfoldr1' :: Linear l e => (e -> e -> e) -> l -> e
sfoldr1' f = \ es' -> case unsnoc' es' of
  Just (es, e) -> sfoldr' f e es
  _            -> pfailEx "sfoldr1'"

{-# WARNING sfoldl1' "This is a partial function, it throws an error on empty lists." #-}

-- | 'sfoldl1'' is just 'Data.Foldable.foldl1'' in 'Linear' context.
sfoldl1' :: Linear l e => (e -> e -> e) -> l -> e
sfoldl1' f = \ es' -> case uncons' es' of
  Just (e, es) -> sfoldl' f e es
  _            -> pfailEx "sfoldl1'"

{- |
  @since 0.3
  
  Same as 'sfoldr', but also returns the length of the structure.
-}
csfoldr' :: Linear l e => (e -> a -> a) -> a -> l -> (Int, a)
csfoldr' f base = sfoldr' (\ e (!n, e') -> (n + 1, f e e')) (0, base)

{- |
  @since 0.3
  
  Same as 'sfoldr', but also returns the length of the structure.
-}
csfoldl' :: Linear l e => (a -> e -> a) -> a -> l -> (Int, a)
csfoldl' f base = sfoldl' (\ (!n, e') e -> (n + 1, f e' e)) (0, base)

{- |
  @since 0.3
  
  Monadic version of 'sfoldr'.
-}
msfoldr :: (Monad m, Linear l e) => (e -> a -> m a) -> a -> l -> m a
msfoldr go = sfoldr (\ e xs -> go e =<< xs) . pure

{- |
  @since 0.3
  
  Monadic version of 'sfoldl'.
-}
msfoldl :: (Monad m, Linear l e) => (a -> e -> m a) -> a -> l -> m a
msfoldl go = sfoldl (\ e xs -> flip go xs =<< e) . pure

--------------------------------------------------------------------------------

{- Extra splits. -}

{- |
  @save n es@ takes first @n@ elements of @es@ if @n > 0@ and last @-n@
  elements otherwise.
-}
save :: Linear l e => Int -> l -> l
save n = n > 0 ? take n $ keep (-n)

{- |
  @skip n es@ drops first @n@ elements of @es@ if @n > 0@ and last @-n@
  elements otherwise.
-}
skip :: Linear l e => Int -> l -> l
skip n = n > 0 ? drop n $ sans (-n)

{- |
  @since 0.3
  
  Splits structures into parts by given offsets.
  
  @
    parts [0,5,6,12,26] ['a'..'z'] = ["","abcde","f","ghijkl","mnopqrstuvwxyz",""]
    -- if previous offset is equal or greater, subline is empty and next
    -- begins from previous:
    parts [0, 5, 4, 12, 26] ['a' .. 'z'] = ["","abcde","","fghijklm","nopqrstuvwxyz",""]
  @
-}
parts :: (Linear l e, Foldable f) => f Int -> l -> [l]
parts =
  let go o is' = case is' of {i : is -> (i - o) : go i is; _ -> []}
  in  splits . go 0 . toList

{- |
  @since 0.3
  
  Splits structures into chunks of size @n@ and the rest.
  
  > chunks x [] = [] -- forall x
  > chunks 0 es = [] -- forall es
  
  > chunks 3 [1 .. 10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
-}
chunks :: Linear l e => Int -> l -> [l]
chunks n es = isNull es || n < 1 ? [] $ let (x, xs) = split n es in x : chunks n xs

--------------------------------------------------------------------------------

{- Extra conditional splits. -}

-- | Left-side span.
spanl :: Linear l e => (e -> Bool) -> l -> (l, l)
spanl p es = (takeWhile p es, dropWhile p es)

-- | Left-side break.
breakl :: Linear l e => (e -> Bool) -> l -> (l, l)
breakl =  spanl . (not .)

{- |
  Split line by first (left) separation element. If there is no such
  element, @splitBy es = (es, Z)@.
  
  > splitBy (== '.') "foo" == ("foo","")
  > splitBy (== '.') "foo." == ("foo","")
  > splitBy (== '.') ".foo" == ("","foo")
  > splitBy (== '.') "foo.bar" == ("foo","bar")
  > splitBy (== '.') "foo.bar.baz" == ("foo","bar.baz")
-}
splitBy :: Linear l e => (e -> Bool) -> l -> (l, l)
splitBy =  second (drop 1) ... breakl

-- | Right-side span.
spanr :: Linear l e => (e -> Bool) -> l -> (l, l)
spanr p es = (dropEnd p es, takeEnd p es)

-- | Right-side break.
breakr :: Linear l e => (e -> Bool) -> l -> (l, l)
breakr =  spanr . (not .)

{- |
  Split line by last (right) separation element. If there is no such
  element, @divide es = (Z, es)@.
  
  > divideBy (== '.') "foo" == ("","foo")
  > divideBy (== '.') ".foo" == ("","foo")
  > divideBy (== '.') "foo." == ("foo","")
  > divideBy (== '.') "foo.bar" == ("foo","bar")
  > divideBy (== '.') "foo.bar.baz" == ("foo.bar","baz")
-}
divideBy :: Linear l e => (e -> Bool) -> l -> (l, l)
divideBy =  first (sans 1) ... breakr

{- |
  @since 0.3
  
  @dropWhileEnd f = dropWhile f . dropEnd f@.
-}
dropWhileEnd :: Linear l e => (e -> Bool) -> l -> l
dropWhileEnd f = dropWhile f . dropEnd f

--------------------------------------------------------------------------------

{- Extra filters. -}

-- | @'except' p es = 'filter' (not . p) es@
except :: Linear l e => (e -> Bool) -> l -> l
except =  filter . (not .)
    
-- | Generalization of 'L.partition'.
partition :: Linear l e => (e -> Bool) -> l -> (l, l)
partition p es = (filter p es, except p es)

-- | Generalization of partition, that select sublines by predicates.
partitions :: (Linear l e, Foldable f) => f (e -> Bool) -> l -> [l]
partitions =  partitions_ . toList
  where
    partitions_    []    es = [es]
    partitions_ (p : ps) es =
      let (f, e) = partition p es
      in  f : partitions_ ps e

--------------------------------------------------------------------------------

{- Extra monadic filters. -}

mfilter :: (Monad m, Linear l e) => (e -> m Bool) -> l -> m l
mfilter go = fmap fromList . mselect (\ e -> do b <- go e; return (b ? Just e $ Z))

{- |
  @since 0.3
  
  Monadic version 'except'.
-}
mexcept :: (Monad m, Linear l e) => (e -> m Bool) -> l -> m l
mexcept =  mfilter . (fmap not .)

-- | Monadic version of 'partition'.
mpartition :: (Monad m, Linear l e) => (e -> m Bool) -> l -> m (l, l)
mpartition p es = liftA2 (,) (mfilter p es) (mexcept p es)

--------------------------------------------------------------------------------

{- Extra scans. -}

-- | 'tails' returns sequence of @es@ 'tail'.
tails :: Linear l e => l -> [l]
tails Z  = [Z]
tails es = es : tails (tail es)

-- | 'inits' returns sequence of @es@  'init'.
inits :: Linear l e => l -> [l]
inits Z  = [Z]
inits es = es : inits (init es)

{- |
  Splits line into sequences of given sizes (left to right).
  
  > splits [5, 3, 12] ['a'..'z'] = ["abcde","fgh","ijklmnopqrst","uvwxyz"]
-}
splits :: (Linear l e, Foldable f) => f Int -> l -> [l]
splits ns es =
  let f = \ es' n -> case es' of
        (r : ds) -> let (d, r') = split n r in r' : d : ds
        _        -> unreachEx "splits: must be non-empty"
  in  reverse $ foldl f [es] ns

{- |
  Splits line into sequences of given sizes (right to left).
  
  > divides [5,3,12] ['a'..'z'] == ["abcdef","ghijk","lmn","opqrstuvwxyz"]
-}
divides :: (Linear l e, Foldable f) => f Int -> l -> [l]
divides ns es =
  let f = \ n es' -> case es' of
        (r : ds) -> let (r', d) = divide n r in r' : d : ds
        _        -> unreachEx "divides: must be non-empty"
  in  foldr f [es] ns

--------------------------------------------------------------------------------

{- Selections. -}

-- | @select f es@ is selective map of @es@ elements to new list.
select :: Linear l e => (e -> Maybe a) -> l -> [a]
select f = sfoldr (\ x es -> maybe es (: es) (f x)) []

-- | @select' f es@ is selective map of @es@ elements to new line.
select' :: Linear l e => (e -> Maybe e) -> l -> l
select' =  fromList ... select

{- |
  @extract f es@ returns a selective map of @es@ elements to new list and
  the remaining elements of the line.
-}
extract :: Linear l e => (e -> Maybe a) -> l -> ([a], [e])
extract f =
  let g = \ b -> second (b :) `maybe` (first . (:)) $ f b
  in  sfoldr' g ([], [])

{- |
  @extract' f es@ returns a selective map of @es@ elements to new line and
  the remaining elements of the line.
-}
extract' :: Linear l e => (e -> Maybe e) -> l -> (l, l)
extract' =  both fromList ... extract

--------------------------------------------------------------------------------

{- Conditional selections. -}

{- |
  @selectWhile f es@ selects results of applying @f@ to @es@ (left to right)
  untill first fail.
-}
selectWhile :: Linear l e => (e -> Maybe a) -> l -> [a]
selectWhile f =
  let go e xs = case f e of {Just x -> x : xs; _ -> []}
  in  sfoldr go []

-- | @selectWhile'@ is 'selectWhile' version for generalized structures.
selectWhile' :: Linear l e => (e -> Maybe e) -> l -> l
selectWhile' =  fromList ... selectWhile

{- |
  @selectEnd f es@ selects results of applying @f@ to @es@ (right to left)
  untill first fail.
-}
selectEnd :: Linear l e => (e -> Maybe a) -> l -> [a]
selectEnd f =
  let go xs e = case f e of {Just x -> x : xs; _ -> []}
  in  reverse . sfoldl go []

-- | @selectEnd'@ is 'selectEnd' version for generalized structures.
selectEnd' :: Linear l e => (e -> Maybe e) -> l -> l
selectEnd' =  fromList ... selectEnd

{- |
  @extractWhile f es@ selects results of applying @f@ to @es@ (left to
  right) untill first fail. Returns selected results and rest of line.
-}
extractWhile :: Linear l e => (e -> Maybe a) -> l -> ([a], l)
extractWhile f es =
  let go e xs = case f e of {Just x -> x : xs; _ -> []}
  in  second (`drop` es) . swap $ csfoldr' go [] es

-- | @extractWhile'@ is 'extractWhile' version for generalized structures.
extractWhile' :: Linear l e => (e -> Maybe e) -> l -> (l, l)
extractWhile' =  first fromList ... extractWhile

{- |
  @extractEnd f es@ selects results of applying @f@ to @es@ (right to left)
  untill first fail. Returns rest of line and selected results.
-}
extractEnd :: Linear l e => (e -> Maybe a) -> l -> (l, [a])
extractEnd f es =
  let go xs e = case f e of {Just x -> x : xs; _ -> []}
  in  bimap (`sans` es) reverse $ csfoldl' go [] es

-- | @extractEnd'@ is 'extractEnd' version for generalized structures.
extractEnd' :: Linear l e => (e -> Maybe e) -> l -> (l, l)
extractEnd' =  second fromList ... extractEnd

--------------------------------------------------------------------------------

{- Monadic selections. -}

{- |
  @since 0.3
  
  Monadic version of 'select'.
-}
mselect :: (Linear l e, Applicative t) => (e -> t (Maybe a)) -> l -> t [a]
mselect go = sfoldr (liftA2 (\ x xs -> maybe xs (: xs) x) . go) (pure [])

{- |
  @since 0.3
  
  Monadic version of 'select''.
-}
mselect' :: (Linear l e, Applicative t) => (e -> t (Maybe e)) -> l -> t l
mselect' =  fmap fromList ... mselect

{- |
  @since 0.3
  
  Monadic version of 'extract'.
-}
mextract :: (Linear l e, Applicative t) => (e -> t (Maybe a)) -> l -> t ([a], [e])
mextract go = sfoldr (\ e -> liftA2 (\ x (xs, es) ->
    maybe (xs, e : es) (\ x' -> (x' : xs, es)) x) (go e)
  ) (pure ([], []))

{- |
  @since 0.3
  
  Monadic version of 'extract''.
-}
mextract' :: (Linear l e, Applicative t) => (e -> t (Maybe e)) -> l -> t (l, l)
mextract' =  fmap (both fromList) ... mextract

--------------------------------------------------------------------------------

-- | @stripPrefix sub line@ strips prefix @sub@ of @line@ (if any).
stripPrefix :: (Linear l e, Eq e) => l -> l -> l
stripPrefix sub line = sub `isPrefixOf` line ? drop (sizeOf sub) line $ line

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ (if any).
stripSuffix :: (Linear l e, Eq e) => l -> l -> l
stripSuffix sub line = sub `isSuffixOf` line ? sans (sizeOf sub) line $ line

-- | @stripPrefix' sub line@ strips prefix @sub@ of @line@ or returns 'Nothing'.
stripPrefix' :: (Linear l e, Eq e) => l -> l -> Maybe l
stripPrefix' sub = isPrefixOf sub ?+ drop (sizeOf sub)

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ or returns 'Nothing'.
stripSuffix' :: (Linear l e, Eq e) => l -> l -> Maybe l
stripSuffix' sub = isSuffixOf sub ?+ sans (sizeOf sub)

--------------------------------------------------------------------------------

-- | Generalized 'intersperse'.
intersperse :: Linear l e => e -> l -> l
intersperse e es =
  let xs = drop 1 $ sfoldr ((e :) ... (:)) [] es
  in  es .< 2 ? es $ fromList xs

-- | intercalate is generalization of intercalate
intercalate :: (Foldable f, Linear l e) => l -> f l -> l
intercalate es = concat . intersperse es . toList

-- | Generalized 'subsequences'.
subsequences :: Linear l e => l -> [l]
subsequences =  map fromList . L.subsequences . listL

{- |
  @each n es@ returns each @n@-th element of structure.
  
  @
    each n [1 .. 5] = []
    each 1 [1 .. 5] = [1 .. 5]
    each 2 [1 .. 5] = [1, 3, 5]
  @
  
  If @n == 1@, returns @es@.
  If @n < 1@, returns 'Z'.
-}
each :: Linear l e => Int -> l -> l
each n es = case n <=> 1 of
  GT -> fromList $ ofoldr (\ i x xs -> mod i n == 0 ? x : xs $ xs) [] es
  EQ -> es
  LT -> Z

{- |
  @eachFrom o n es@ returns each nth element of structure, beginning from o.
  
  @
    eachFrom o n = each n . drop o
    eachFrom 0 2 [1 .. 20] == [2, 4 .. 20]
    eachFrom 1 2 [1 .. 20] == [3, 5 .. 19]
  @
-}
eachFrom :: Linear l e => Int -> Int -> l -> l
eachFrom o n = each n . drop o

{- |
  @ascending es lengths@ checks if the subsequences of @es@ of lengths @lengths@
  is sorted.
-}
ascending :: (Sort l e, Linear l e, Ord e) => l -> [Int] -> Bool
ascending =  all sorted ... flip splits

--------------------------------------------------------------------------------

{-# COMPLETE [], (:>) #-}
{-# COMPLETE [], (:<) #-}

instance Linear [e] e
  where
    toHead = (:)
    toLast = flip (foldr' (:) . pure)
    
    uncons' = L.uncons
    unsnoc' = foldr (\ x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
    -- unsnoc' = L.unsnoc @since base-4.19.0.0
    
    head (x : _) = x
    head   [ ]   = pfailEx "head"
    
    tail (_ : xs) = xs
    tail    []    = pfailEx "tail"
    
    init  = L.init
    last  = L.last
    
    single       = pure
    fromList     = id
    fromListN    = L.take
    fromFoldable = toList
    replicate    = L.replicate
    
    (!!) = (L.!!)
    
    write es n e = n < 0 ? es $ go n es
      where
        go i (x : xs) = i == 0 ? e : xs $ x : go (i - 1) xs
        go _ _ = []
    
    nubBy      = L.nubBy
    filter     = L.filter
    reverse    = L.reverse
    unfoldr    = L.unfoldr
    isSubseqOf = L.isSubsequenceOf
    
    remove i es = i < 0 ? es $ go i es
      where
        go 0 (_ : xs) = xs
        go n (x : xs) = x : go (n - 1) xs
        go _    []    = []
    
    take  = L.take
    drop  = L.drop
    split = L.splitAt
    
    infixes  Z  = const []
    infixes sub = go 0
      where
        go _ [] = []
        go i es = sub `isPrefixOf` es ? i : go (i + n) (drop n es) $ go (i + 1) (tail es)
        
        n = sizeOf sub
    
    splitsBy f es = dropWhile f <$> L.findIndices f es `parts` es
    
    isPrefixOf = L.isPrefixOf
    isSuffixOf = L.isSuffixOf
    isInfixOf  = L.isInfixOf

--------------------------------------------------------------------------------

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Linear."

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Linear."


