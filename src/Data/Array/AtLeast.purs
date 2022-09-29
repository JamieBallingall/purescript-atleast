module Data.Array.ArrayAL
  ( ArrayAL -- Constructor not exported. Use fromArray, fromArray' or similar

  , fromArray
  , fromArray'
  , fromNonEmptyArray
  , empty
  , solo
  , duet
  , trio
  , quartet
  , quintet
  , solo'
  , duet'
  , trio'
  , quartet'
  , quintet'
  , replicate
  , cons
  , snoc
  , append
  , appendArray
  , prependArray
  , range
  , concat
  , intersperse

  , toArray
  , toNonEmptyArray
  , upper
  , lower
  , uncons
  , unsnoc

  , length
  , index
  , partialIndex
  , indexUpper
  , head
  , last

  , tail
  , init
  , take
  , takeEnd

  , reverse
  , sort
  , sortBy
  , sortWith

  , catMaybes
  , nub
  , nubEq
  , nubBy
  , nubByEq

  , zipWith
  , offsetWith

  , checkValid
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.FastVect.FastVect as FV
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Int.AtLeast (IntAL, fromInt', toInt)
import Data.Maybe (Maybe(Nothing, Just), fromJust)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Semigroup.Foldable (class Foldable1, foldMap1DefaultL)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Partial.Unsafe (unsafePartial)
import Prim.Int (class Add, class Compare, class Mul)
import Prim.Ordering (EQ, GT, LT)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype ArrayAL :: Int -> Type -> Type
-- | An `Array` with a minimum, but not maximum, length as specified in the type
-- | `ArrayAL 0` is isomorphic to `Array`, `ArrayAL 1` to `NonEmptyArray` and
-- | `ArrayAL 2` to `TwoOrMore`. We refer to the part of the array guaranteed by
-- | the type system as the "upper" part and the remainder as the "lower" part.
-- | So an `ArrayAL 5` has exactly 5 elements in the upper part and zero or more
-- | elements in the lower part
newtype ArrayAL n a = ArrayAL (Array a)

instance (Show a, Reflectable n Int) => Show (ArrayAL n a) where
  show (ArrayAL xs) =
    "ArrayAL " <> show (reflectType (Proxy :: Proxy n)) <> " " <> show xs

derive newtype instance Eq a => Eq (ArrayAL n a)
derive newtype instance Ord a => Ord (ArrayAL n a)
derive newtype instance Functor (ArrayAL n)
derive newtype instance FunctorWithIndex Int (ArrayAL n)
derive newtype instance Foldable (ArrayAL n)
derive newtype instance FoldableWithIndex Int (ArrayAL n)
derive newtype instance Traversable (ArrayAL n)
derive newtype instance TraversableWithIndex Int (ArrayAL n)

instance Compare n 0 GT => Foldable1 (ArrayAL n) where
  foldMap1 = foldMap1DefaultL
  foldr1 = foldr1Impl
  foldl1 = foldl1Impl

foreign import foldr1Impl :: forall n a. (a -> a -> a) -> ArrayAL n a -> a
foreign import foldl1Impl :: forall n a. (a -> a -> a) -> ArrayAL n a -> a

instance Apply (ArrayAL n) where
  apply (ArrayAL fab) (ArrayAL a) = ArrayAL (Array.zipWith ($) fab a)

instance
  ( Compare n (-1) GT
  , Reflectable n Int
  ) =>
  Applicative (ArrayAL n) where
  pure = replicate (Proxy :: Proxy n)

-- Functions that construct ArrayALs -------------------------------------------

-- | Convert an `Array` to an `ArrayAL`, returning `Nothing` if that is not
-- | possible
fromArray
  :: ∀ (n :: Int) (a :: Type)
   . Compare n (-1) GT
  => Reflectable n Int
  => Proxy n
  -> Array a
  -> Maybe (ArrayAL n a)
fromArray len xs =
  if Array.length xs >= reflectType len then Just $ ArrayAL xs else Nothing

-- | Convert an `Array` that you are confident is sufficiently long to an
-- | `ArrayAL`. Note the `Partial` constraint. Will not crash immediately if
-- | the `Partial` constraint is erroneously discharged but will create an
-- | invalid `ArrayAL` that is liable to cause other functions to crash.
fromArray' :: ∀ (n :: Int) (a :: Type). Partial => Array a -> ArrayAL n a
fromArray' = unsafeCoerce

-- | Convert a `NonEmptyArray` to an `ArrayAL 1`
fromNonEmptyArray :: ∀ a. NonEmptyArray a -> ArrayAL 1 a
fromNonEmptyArray = unsafeCoerce

-- | Construct an empty `ArrayAL 0`
empty :: ∀ (a :: Type). ArrayAL 0 a
empty = ArrayAL []

-- | Construct an `ArrayAL 1` with exactly one element. Called `solo` rather
-- | than `singleton` to be consistent with other constructors that use musical
-- | conventions: solo, duet, trio, quartet and quintet
solo :: ∀ (a :: Type). a -> ArrayAL 1 a
solo x = ArrayAL [ x ]

-- | Construct an `ArrayAL 2` with exactly two elements
duet :: ∀ (a :: Type). a -> a -> ArrayAL 2 a
duet x1 x2 = ArrayAL [ x1, x2 ]

-- | Construct an `ArrayAL 3` with exactly three elements
trio :: ∀ (a :: Type). a -> a -> a -> ArrayAL 3 a
trio x1 x2 x3 = ArrayAL [ x1, x2, x3 ]

-- | Construct an `ArrayAL 4` with exactly four elements
quartet :: ∀ (a :: Type). a -> a -> a -> a -> ArrayAL 4 a
quartet x1 x2 x3 x4 = ArrayAL [ x1, x2, x3, x4 ]

-- | Construct an `ArrayAL 5` with exactly five elements
quintet :: ∀ (a :: Type). a -> a -> a -> a -> a -> ArrayAL 5 a
quintet x1 x2 x3 x4 x5 = ArrayAL [ x1, x2, x3, x4, x5 ]

-- | Construct an `ArrayAL 1` with one or more elements. The first element is
-- | explicitly provided and the remaining elements are provided in an `Array`
solo' :: ∀ (a :: Type). a -> Array a -> ArrayAL 1 a
solo' x xs = ArrayAL $ Array.cons x xs

-- | Construct an `ArrayAL 2` with two or more elements. The first two elements
-- | are explicitly provided and the remaining elements are provided in an
-- | `Array`
duet' :: ∀ (a :: Type). a -> a -> Array a -> ArrayAL 2 a
duet' x1 x2 xs = ArrayAL $ [ x1, x2 ] <> xs

-- | Construct an `ArrayAL 3` with three or more elements. The first three
-- | elements are explicitly provided and the remaining elements are provided in
-- | an `Array`
trio' :: ∀ (a :: Type). a -> a -> a -> Array a -> ArrayAL 3 a
trio' x1 x2 x3 xs = ArrayAL $ [ x1, x2, x3 ] <> xs

-- | Construct an `ArrayAL 4` with four or more elements. The first four
-- | elements are explicitly provided and the remaining elements are provided in
-- | an `Array`
quartet' :: ∀ (a :: Type). a -> a -> a -> a -> Array a -> ArrayAL 4 a
quartet' x1 x2 x3 x4 xs = ArrayAL $ [ x1, x2, x3, x4 ] <> xs

-- | Construct an `ArrayAL 5` with three or more elements. The first five
-- | elements are explicitly provided and the remaining elements are provided in
-- | an `Array`
quintet' :: ∀ (a :: Type). a -> a -> a -> a -> a -> Array a -> ArrayAL 5 a
quintet' x1 x2 x3 x4 x5 xs = ArrayAL $ [ x1, x2, x3, x4, x5 ] <> xs

-- | Create an `ArrayAL n` containing a value repeated the specified number of
-- | times. If
replicate
  :: ∀ (n :: Int) (a :: Type)
   . Compare n (-1) GT
  => Reflectable n Int
  => Proxy n
  -> a
  -> ArrayAL n a
replicate _ element =
  ArrayAL $ Array.replicate (reflectType (Proxy :: _ n)) element

-- | Attach an element to the start of an `ArrayAL n`, creating a new
-- | `ArrayAL (n + 1)`. Note that the running time of this function is `O(n)`.
cons
  :: ∀ (n :: Int) (n_plus_1 :: Int) (a :: Type)
   . Add n 1 n_plus_1
  => a
  -> ArrayAL n a
  -> ArrayAL n_plus_1 a
cons x (ArrayAL xs) = ArrayAL (Array.cons x xs)

-- | An infix alias for cons.
infixr 6 cons as :

-- | Attach an element to the end of an `ArrayAL n`, creating a new
-- | `ArrayAL (n + 1)`
snoc
  :: ∀ (n :: Int) (n_plus_1 :: Int) (a :: Type)
   . Add n 1 n_plus_1
  => ArrayAL n a
  -> a
  -> ArrayAL n_plus_1 a
snoc (ArrayAL xs) x = ArrayAL (Array.snoc xs x)

append
  :: ∀ (n1 :: Int) (n2 :: Int) (n3 :: Int) (a :: Type)
   . Add n1 n2 n3
  => ArrayAL n1 a
  -> ArrayAL n2 a
  -> ArrayAL n3 a
append (ArrayAL xs) (ArrayAL ys) = ArrayAL $ xs <> ys

appendArray :: ∀ (n :: Int) (a :: Type). ArrayAL n a -> Array a -> ArrayAL n a
appendArray (ArrayAL xs) ys = ArrayAL $ xs <> ys

prependArray :: ∀ (n :: Int) (a :: Type). Array a -> ArrayAL n a -> ArrayAL n a
prependArray xs (ArrayAL ys) = ArrayAL $ xs <> ys

-- | Construct an `ArrayAL 1` of `IntAL min`, starting with the (type-level)
-- | minimum `min` and ending with the runtime value of the `IntAL min`
range
  :: ∀ (min :: Int). Reflectable min Int => IntAL min -> ArrayAL 1 (IntAL min)
range i = ArrayAL $ toIntAL <$> values
  where

  toIntAL :: Int -> IntAL min
  toIntAL j = unsafePartial $ fromInt' j

  values :: Array Int
  values = Array.range (reflectType (Proxy :: _ min)) (toInt i)

concat
  :: ∀ (m :: Int) (n :: Int) (mn :: Int) (a :: Type)
   . Compare (-1) m LT
  => Compare (-1) n LT
  => Mul m n mn
  => ArrayAL m (ArrayAL n a)
  -> ArrayAL mn a
concat (ArrayAL xss) =
  let
    rawArrays = toArray <$> xss
  in
    ArrayAL $ Array.concat rawArrays

intersperse
  :: ∀ (n1 :: Int) (n2 :: Int) (two_n1 :: Int) (a :: Type)
   . Mul 2 n1 two_n1
  => Add (-1) two_n1 n2
  => a
  -> ArrayAL n1 a
  -> ArrayAL n2 a
intersperse x (ArrayAL xs) = ArrayAL $ Array.intersperse x xs

-- Functions that deconstruct ArrayALs -----------------------------------------

-- | Convert an `ArrayAL` to an `Array`
toArray :: ∀ (n :: Int) (a :: Type). ArrayAL n a -> Array a
toArray = unsafeCoerce

-- | Convert an `ArrayAL n` to a `toNonEmptyArray`. `n` is required to be at
-- | least 1
toNonEmptyArray :: ∀ n a. Compare n 0 GT => ArrayAL n a -> NonEmptyArray a
toNonEmptyArray = unsafeCoerce

-- | Extract the upper part of the `ArrayAL n` as a `Vect n`
upper
  :: ∀ n a
   . Reflectable n Int
  => Compare n (-1) GT
  => ArrayAL n a
  -> FV.Vect n a
upper (ArrayAL xs) =
  let
    nProxy = Proxy :: Proxy n
    nInt = reflectType nProxy
    as = Array.take nInt xs
  in
    unsafePartial $ fromJust $ FV.fromArray nProxy as

-- | Extract the lower part of the `ArrayAL n` as a (possibly empty) `Array`
lower
  :: ∀ n a
   . Reflectable n Int
  => Compare n (-1) GT
  => ArrayAL n a
  -> Array a
lower (ArrayAL xs) = Array.drop (reflectType (Proxy :: Proxy n)) xs

uncons
  :: ∀ (n :: Int) (n_minus_1 :: Int) (a :: Type)
   . Compare 0 n LT
  => Add 1 n_minus_1 n
  => ArrayAL n a
  -> { head :: a, tail :: ArrayAL n_minus_1 a }
uncons (ArrayAL xs) =
  let
    raw = definitely $ Array.uncons xs
  in
    { head: raw.head, tail: ArrayAL raw.tail }

unsnoc
  :: ∀ (n :: Int) (n_minus_1 :: Int) (a :: Type)
   . Compare 0 n LT
  => Add 1 n_minus_1 n
  => ArrayAL n a
  -> { init :: ArrayAL n_minus_1 a, last :: a }
unsnoc (ArrayAL xs) =
  let
    raw = definitely $ Array.unsnoc xs
  in
    { init: ArrayAL raw.init, last: raw.last }

-- Extract individual elements of an `ArrayAL` ---------------------------------

-- | The number of elements in an `ArrayAL n`, as an `IntAL n`
length
  :: ∀ (n :: Int) (a :: Type)
   . Reflectable n Int
  => ArrayAL n a
  -> IntAL n
length (ArrayAL xs) = unsafePartial $ fromInt' $ Array.length xs

-- | Safely read a value at a particular index from an `ArrayAL`, returning
-- | Nothing if it does not exist
index :: ∀ n a. ArrayAL n a -> Int -> Maybe a
index (ArrayAL xs) idx = Array.index xs idx

-- | An infix version of `index`
infixl 8 index as !!

-- | Read a value at a particular index. Note the `Partial` constraint. Will not
-- | crash if the `Partial` constraint is erroneously discharged but subsequent
-- | behaviour will be undefined
partialIndex :: ∀ (n :: Int) (a :: Type). Partial => ArrayAL n a -> Int -> a
partialIndex (ArrayAL xs) i = rawIndex i xs

-- | Safely read a value at a particular index from the upper part of an
-- | `ArrayAL`. The index is provided as a `Proxy` to an `Int` so it can be
-- | checked by the compiler
indexUpper
  :: ∀ n n_minus_1 idx n_minus_idx_minus_1 a
   . Reflectable idx Int
  => Add 1 n_minus_1 n
  => Add idx n_minus_idx_minus_1 n_minus_1
  => Compare (-1) n_minus_1 LT
  => Compare (-1) n_minus_idx_minus_1 LT
  => Compare (-1) idx LT
  => Proxy idx
  -> ArrayAL n a
  -> a
indexUpper i (ArrayAL xs) = rawIndex (reflectType i) xs

head :: ∀ (n :: Int) (a :: Type). Compare 0 n LT => ArrayAL n a -> a
head (ArrayAL xs) = rawIndex 0 xs

last :: ∀ (n :: Int) (a :: Type). Compare 0 n LT => ArrayAL n a -> a
last (ArrayAL xs) = definitely $ Array.last xs

-- Functions that extract parts of an `ArrayAL` --------------------------------
tail
  :: ∀ (n :: Int) (n_minus_1 :: Int) (a :: Type)
   . Compare 0 n LT
  => Add 1 n_minus_1 n
  => ArrayAL n a
  -> ArrayAL n_minus_1 a
tail (ArrayAL xs) = ArrayAL $ definitely $ Array.tail xs

init
  :: ∀ (n :: Int) (n_minus_1 :: Int) (a :: Type)
   . Compare 0 n LT
  => Add 1 n_minus_1 n
  => ArrayAL n a
  -> ArrayAL n_minus_1 a
init (ArrayAL xs) = ArrayAL $ definitely $ Array.init xs

take
  :: ∀ (m :: Int) (m1 :: Int) (n :: Int) (a :: Type)
   . Add m 1 m1
  => Compare n m1 LT
  => Compare (-1) n LT
  => Reflectable n Int
  => ArrayAL m a
  -> FV.Vect n a
take (ArrayAL xs) =
  let
    nProxy = Proxy :: _ n
    nInt = reflectType nProxy
  in
    unsafePartial $ fromJust $ FV.fromArray nProxy $ Array.take nInt xs

takeEnd
  :: ∀ (m :: Int) (m1 :: Int) (n :: Int) (a :: Type)
   . Add m 1 m1
  => Compare n m1 LT
  => Compare (-1) n LT
  => Reflectable n Int
  => ArrayAL m a
  -> FV.Vect n a
takeEnd (ArrayAL xs) =
  let
    nProxy = Proxy :: _ n
    nInt = reflectType nProxy
  in
    unsafePartial $ fromJust $ FV.fromArray nProxy $ Array.takeEnd nInt xs

-- | An infix version of `index`
infixl 8 indexUpper as !!^

-- Operations on `ArrayAL` that do not change the length -----------------------

reverse :: ∀ (n :: Int) (a :: Type). ArrayAL n a -> ArrayAL n a
reverse (ArrayAL xs) = ArrayAL $ Array.reverse xs

sort :: ∀ (n :: Int) (a :: Type). Ord a => ArrayAL n a -> ArrayAL n a
sort (ArrayAL xs) = ArrayAL $ Array.sort xs

sortBy
  :: ∀ (n :: Int) (a :: Type)
   . (a -> a -> Ordering)
  -> ArrayAL n a
  -> ArrayAL n a
sortBy comp (ArrayAL xs) = ArrayAL $ Array.sortBy comp xs

sortWith
  :: ∀ (n :: Int) (a :: Type) (b :: Type)
   . Ord b
  => (a -> b)
  -> ArrayAL n a
  -> ArrayAL n a
sortWith f (ArrayAL xs) = ArrayAL $ Array.sortWith f xs

-- Operations on `ArrayAL` that may shrink the `ArrayAL` -----------------------
catMaybes :: ∀ (n :: Int) (a :: Type). ArrayAL n (Maybe a) -> ArrayAL 0 a
catMaybes (ArrayAL xs) = ArrayAL $ Array.catMaybes xs

nub
  :: ∀ (n :: Int) (a :: Type) (b :: Type)
   . Compare 0 n LT
  => Ord a
  => ArrayAL n a
  -> ArrayAL 1 a
nub (ArrayAL xs) = ArrayAL $ Array.nub xs

nubEq
  :: ∀ (n :: Int) (a :: Type) (b :: Type)
   . Compare 0 n LT
  => Eq a
  => ArrayAL n a
  -> ArrayAL 1 a
nubEq (ArrayAL xs) = ArrayAL $ Array.nubEq xs

nubBy
  :: ∀ (n :: Int) (a :: Type)
   . Compare 0 n LT
  => (a -> a -> Ordering)
  -> ArrayAL n a
  -> ArrayAL 1 a
nubBy f (ArrayAL xs) = ArrayAL $ Array.nubBy f xs

nubByEq
  :: ∀ (n :: Int) (a :: Type)
   . Compare 0 n LT
  => (a -> a -> Boolean)
  -> ArrayAL n a
  -> ArrayAL 1 a
nubByEq f (ArrayAL xs) = ArrayAL $ Array.nubByEq f xs

-- Zipper functions operating on two `ArrayAL`s or on the same `ArrayAL` twice

-- | Apply a function to pairs of elements at the same index in two arrays,
-- | collecting the results in a new array. If one array is longer, elements
-- | will be discarded from the longer array. The function `zip` is not
-- | implemented, use `zipWith Tuple`
zipWith
  :: ∀ n a b c
   . (a -> b -> c)
  -> ArrayAL n a
  -> ArrayAL n b
  -> ArrayAL n c
zipWith fn (ArrayAL xs) (ArrayAL ys) = ArrayAL $ Array.zipWith fn xs ys

foreign import offsetWithImpl
  :: ∀ a b. Int -> (a -> a -> b) -> Array a -> Array b

-- | Apply a binary function to offset copies of an `ArrayAL`
offsetWith
  :: ∀ (n1 :: Int)
       (n2 :: Int)
       (offset :: Int)
       (n2_plus_offset :: Int)
       (a1 :: Type)
       (a2 :: Type)
   . Reflectable offset Int
  => Add n2 offset n2_plus_offset
  => Compare n1 (-1) GT
  => Compare offset (-1) GT
  => Compare n1 n2_plus_offset EQ
  => Proxy offset
  -> (a1 -> a1 -> a2)
  -> ArrayAL n1 a1
  -> ArrayAL n2 a2
offsetWith i f (ArrayAL xs) = ArrayAL $ offsetWithImpl (reflectType i) f xs

-- | Return true if the length of the underlying array is less than or equal to
-- | the type-level minimum length. This should always be true and this function
-- | is provided mostly for testing and debugging purposes
checkValid
  :: ∀ (n :: Int) (a :: Type). Reflectable n Int => ArrayAL n a -> Boolean
checkValid (ArrayAL xs) = Array.length xs >= reflectType (Proxy :: _ n)

-- Not exported. Internal use only ---------------------------------------------
-- An unsafe function to read a value at a particular index from an `Array`
foreign import rawIndex :: ∀ a. Int -> Array a -> a

-- Force a Maybe value to a value
definitely :: ∀ (a :: Type). Maybe a -> a
definitely x = unsafePartial $ fromJust x
