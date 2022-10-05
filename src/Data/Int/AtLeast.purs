module Data.Int.AtLeast
  ( IntAL -- Constructor not exported. Use fromInt, fromInt' or pullUp
  , fromInt
  , fromInt'
  , pullUp
  , fromMin
  , clamp
  , fromLength
  , toInt
  , toNumber
  , extent
  , (%+%)
  , plus
  , (%-%)
  , minus
  , (%*%)
  , times
  , (%/%)
  , quotient
  , modulo
  , remainder
  , divide
  , weaken
  , strengthen
  , pullUp'
  , gcd
  , lcm
  , lengthArray
  , lengthNonEmptyArray
  , checkValid
  ) where

import Prelude hiding (lcm, gcd)

import Data.Array (length, range) as Array
import Data.Array.NonEmpty (NonEmptyArray, cons', length, singleton) as NEA
import Data.Enum (class Enum)
import Data.EuclideanRing (gcd) as Int
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Nothing, Just))
import Data.Reflectable (class Reflectable, reflectType)
import Partial (crashWith)
import Prim.Int (class Add, class Compare, class Mul)
import Prim.Ordering (LT)
import Prim.TypeError (class Warn, Text)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, elements, oneOf)
import Type.Proxy (Proxy(..))

newtype IntAL :: Int -> Type
-- | An `Int` that is always at least some type-level value
newtype IntAL min = IntAL Int

derive instance Eq (IntAL min)

derive instance Ord (IntAL min)

instance Reflectable min Int => Bounded (IntAL min) where
  bottom = IntAL $ reflectType (Proxy :: _ min)
  top = IntAL top

instance Reflectable min Int => Enum (IntAL min) where
  succ (IntAL n) = if n < top then Just (IntAL $ n + 1) else Nothing
  pred (IntAL n) =
    if n > reflectType (Proxy :: _ min) then Just (IntAL $ n - 1) else Nothing

instance Reflectable min Int => Show (IntAL min) where
  show (IntAL i) = "IntAL " <> show (reflectType (Proxy :: _ min)) <> " " <>
    show i

instance Reflectable min Int => Arbitrary (IntAL min) where
  arbitrary =
    let
      n = reflectType (Proxy :: _ min)
    in
      IntAL <$> oneOf
        ( NEA.cons'
            (elements $ NEA.singleton n)
            [ chooseInt (n + 1) (n + 10)
            , chooseInt (n + 11) (n + 100)
            , chooseInt (n + 101) (n + 10000)
            ]
        )

-- | Convert an `Int` to an `IntAL min`, returning `Nothing` if the runtime
-- | value of the input is smaller than `min`
fromInt :: ∀ (min :: Int). Reflectable min Int => Int -> Maybe (IntAL min)
fromInt i =
  if i >= reflectType (Proxy :: _ min) then Just (IntAL i) else Nothing

-- | A partial function, converting an `Int` to an `IntAL min`. Crashes at
-- | runtime if the runtime value of the input is smaller than `min`
fromInt'
  :: ∀ (min :: Int). Reflectable min Int => Partial => Int -> IntAL min
fromInt' i =
  let
    minInt = reflectType (Proxy :: _ min)
  in
    if i >= minInt then IntAL i
    else crashWith $
      "Cannot convert Int " <> show i <> " to IntAL " <> show minInt

-- | Convert an `Int` to an `IntAL`, increasing the value to the type-level
-- | minimum if required
pullUp :: ∀ (min :: Int). Reflectable min Int => Int -> IntAL min
pullUp i =
  let
    minInt = reflectType (Proxy :: _ min)
  in
    IntAL $ if i >= minInt then i else minInt

-- | Construct an `IntAL` equal to its type-level minimum
fromMin :: ∀ (min :: Int). Reflectable min Int => IntAL min
fromMin = IntAL $ reflectType (Proxy :: _ min)

-- | Convert an `Int` to an `IntAL`, increasing the value to the type-level
-- | minimum if required
clamp
  :: ∀ (min :: Int)
   . Warn (Text "`clamp` is deprecated in favor of `pullUp`")
  => Reflectable min Int
  => Int
  -> IntAL min
clamp i =
  let
    minInt = reflectType (Proxy :: _ min)
  in
    if i >= minInt then IntAL i else IntAL minInt

-- | Construct an `Array` of `IntAL`. To construct an `ArrayAL` of `IntAL` see
-- | Data.Array.AtLeast.fromLength'
fromLength
  :: ∀ (min_val :: Int) (min_len :: Int)
   . Compare (-1) min_len LT
  => IntAL min_val
  -> IntAL min_len
  -> Array (IntAL min_val)
fromLength (IntAL first) (IntAL len) =
  if len == 0 then []
  else IntAL <$> Array.range first (first + len - 1)

-- | Convert an `IntAL` to an `Int`
toInt :: ∀ (min :: Int). IntAL min -> Int
toInt (IntAL i) = i

-- | Convert an `IntAL` to a Number
toNumber :: ∀ (min :: Int). IntAL min -> Number
toNumber (IntAL i) = Int.toNumber i

-- | Measure the distance between the runtime value of an `IntAL min` and the
-- | type-level minimum value `min`, as an IntAL 0
extent :: ∀ (min :: Int). Reflectable min Int => IntAL min -> IntAL 0
extent (IntAL i) = IntAL $ reflectType (Proxy :: _ min) - i

-- | Add two IntAL values with possibly different type-level minimums
plus
  :: ∀ (min_i :: Int) (min_j :: Int) (min_sum :: Int)
   . Add min_i min_j min_sum
  => IntAL min_i
  -> IntAL min_j
  -> IntAL min_sum
plus (IntAL i) (IntAL j) = IntAL $ i + j

infixl 6 plus as %+%

-- | Subtract an IntAL value from another IntAL value, returning an Int
minus :: ∀ (min_i :: Int) (min_j :: Int). IntAL min_i -> IntAL min_j -> Int
minus (IntAL i) (IntAL j) = i - j

infixl 6 minus as %-%

-- | Multiply two IntAL values with possibly different type-level minimums.
-- | Limited to types with non-negative type-level minimums
times
  :: ∀ (min_i :: Int) (min_j :: Int) (min_product :: Int)
   . Compare (-1) min_i LT
  => Compare (-1) min_j LT
  => Mul min_i min_j min_product
  => IntAL min_i
  -> IntAL min_j
  -> IntAL min_product
times (IntAL i) (IntAL j) = IntAL $ i * j

infixl 7 times as %*%

-- | Divide one `IntAL` by another, returning an `IntAL 0`. Both parameters must
-- | have non-negative type-level minimums. Similar to `div` by accepts and
-- | returns `IntAL`
quotient
  :: ∀ (min_i :: Int) (min_j :: Int)
   . Compare (-1) min_i LT
  => Compare (-1) min_j LT
  => IntAL min_i
  -> IntAL min_j
  -> IntAL 0
quotient (IntAL i) (IntAL j) = IntAL $ div i j

infixl 7 quotient as %/%

-- | Compute the remainder after division of one `IntAL` by another, returning
-- | an `IntAL 0`. Similar to `mod` by accepts and returns `IntAL 0`
modulo :: ∀ (min_i :: Int) (min_j :: Int). IntAL min_i -> IntAL min_j -> IntAL 0
modulo (IntAL i) (IntAL j) = remainder i j

-- | Compute the remainder after division of one `Int` by another, returning an
-- | `IntAL 0`. Identical to `mod` but returns an `IntAL 0`
remainder :: Int -> Int -> IntAL 0
remainder i j = IntAL (mod i j)

-- | Divide one `IntAL` by another, returning a Number
divide :: ∀ (min_i :: Int) (min_j :: Int). IntAL min_i -> IntAL min_j -> Number
divide i j = toNumber i / toNumber j

-- | Decrease the type-level minimum value without changing the runtime value
weaken
  :: ∀ (min_from :: Int) (min_to :: Int) (min_from_plus_one :: Int)
   . Add 1 min_from min_from_plus_one
  => Compare min_to min_from_plus_one LT
  => IntAL min_from
  -> IntAL min_to
weaken (IntAL i) = IntAL i

-- | Increase the type-level minimum value without changing the runtime value,
-- | returning Nothing if that is not possible
strengthen
  :: ∀ (min_from :: Int) (min_to :: Int)
   . Reflectable min_to Int
  => IntAL min_from
  -> Maybe (IntAL min_to)
strengthen (IntAL i) = fromInt i

-- | Increase the type-level minimum value, increasing the runtime value if
-- | necessary
pullUp'
  :: ∀ (min_from :: Int) (min_to :: Int)
   . Reflectable min_to Int
  => IntAL min_from
  -> IntAL min_to
pullUp' (IntAL i) = pullUp i

-- | The greatest common divisor of two IntAL values with possibly different
-- | type-level minimums. Limited to types with strictly positive type-level
-- | minimums. Returns an IntAL 1
gcd
  :: ∀ (min_i :: Int) (min_j :: Int)
   . Compare 0 min_i LT
  => Compare 0 min_j LT
  => IntAL min_i
  -> IntAL min_j
  -> IntAL 1
gcd (IntAL i) (IntAL j) = IntAL $ Int.gcd i j

-- | The least common multiple of two IntALs values with possibly different
-- | type-level minimums. Limited to types with strictly positive type-level
-- | minimums. The value of the type with the lower type-level minimum must be
-- | provided first. The return type has a type-level minimum equal to that of
-- | the second (higher) input
lcm
  :: ∀ (min_i :: Int) (min_j :: Int) (min_j_plus_one :: Int)
   . Add 1 min_j min_j_plus_one
  => Compare 0 min_i LT
  => Compare min_i min_j_plus_one LT
  => IntAL min_i
  -> IntAL min_j
  -> IntAL min_j
lcm (IntAL i) (IntAL j) = IntAL $ i * (j / Int.gcd i j)

-- | The length of an `Array` as an `IntAL 0`
lengthArray :: ∀ (a :: Type). Array a -> IntAL 0
lengthArray xs = IntAL $ Array.length xs

-- | The length of a `NonEmptyArray` as an `IntAL 1`
lengthNonEmptyArray :: ∀ (a :: Type). NEA.NonEmptyArray a -> IntAL 1
lengthNonEmptyArray xs = IntAL $ NEA.length xs

-- | Return true if the runtime value is greater than or equal to the type-level
-- | value. Should always return true. Used mostly for testing.
checkValid :: ∀ min. Reflectable min Int => IntAL min -> Boolean
checkValid (IntAL i) = i >= reflectType (Proxy :: _ min)
