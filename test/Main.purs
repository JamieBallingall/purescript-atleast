module Test.Main
  ( main
  ) where

import Data.Int.AtLeast

import Data.Maybe (Maybe(Nothing, Just))
import Data.Reflectable (class Reflectable)
import Effect (Effect)
import Prelude (Unit, (==), (+), ($), discard, negate)
import Prim.Int (class Add, class Mul, class Compare)
import Prim.Ordering (LT)
import Test.QuickCheck (quickCheck')
import Type.Proxy (Proxy(Proxy))

fromIntValid
  :: ∀ (min :: Int). Reflectable min Int => Proxy (IntAL min) -> Effect Unit
fromIntValid _ = quickCheck' n test
  where
  test i = nothingOrValid (fromInt i :: Maybe (IntAL min))

plusValid
  :: ∀ (min_i :: Int) (min_j :: Int) (min_sum :: Int)
   . Add min_i min_j min_sum
  => Reflectable min_i Int
  => Reflectable min_j Int
  => Reflectable min_sum Int
  => Proxy (IntAL min_i)
  -> Proxy (IntAL min_j)
  -> Effect Unit
plusValid _ _ = quickCheck' n test
  where
  test :: IntAL min_i -> IntAL min_j -> Boolean
  test i j = checkValid (i %+% j)

plusCompatibleWithAdd
  :: ∀ (min_i :: Int) (min_j :: Int) (min_sum :: Int)
   . Add min_i min_j min_sum
  => Reflectable min_i Int
  => Reflectable min_j Int
  => Reflectable min_sum Int
  => Proxy (IntAL min_i)
  -> Proxy (IntAL min_j)
  -> Effect Unit
plusCompatibleWithAdd _ _ = quickCheck' n test
  where
  test :: IntAL min_i -> IntAL min_j -> Boolean
  test i j = toInt (i %+% j) == (toInt i) + (toInt j)

timesValid
  :: ∀ (min_i :: Int) (min_j :: Int) (min_product :: Int)
   . Mul min_i min_j min_product
  => Compare (-1) min_i LT
  => Compare (-1) min_j LT
  => Reflectable min_i Int
  => Reflectable min_j Int
  => Reflectable min_product Int
  => Proxy (IntAL min_i)
  -> Proxy (IntAL min_j)
  -> Effect Unit
timesValid _ _ = quickCheck' n test
  where
  test :: IntAL min_i -> IntAL min_j -> Boolean
  test i j = checkValid (i %*% j)

timesCompatibleWithMul
  :: ∀ (min_i :: Int) (min_j :: Int) (min_product :: Int)
   . Mul min_i min_j min_product
  => Compare (-1) min_i LT
  => Compare (-1) min_j LT
  => Reflectable min_i Int
  => Reflectable min_j Int
  => Reflectable min_product Int
  => Proxy (IntAL min_i)
  -> Proxy (IntAL min_j)
  -> Effect Unit
timesCompatibleWithMul _ _ = quickCheck' n test
  where
  test :: IntAL min_i -> IntAL min_j -> Boolean
  test i j = checkValid (i %*% j)

weakenValid
  :: ∀ (min_from :: Int) (min_to :: Int) (min_from_plus_one :: Int)
   . Add 1 min_from min_from_plus_one
  => Compare min_to min_from_plus_one LT
  => Reflectable min_from Int
  => Reflectable min_to Int
  => Proxy (IntAL min_from)
  -> Proxy (IntAL min_to)
  -> Effect Unit
weakenValid _ _ = quickCheck' n test
  where
  test :: IntAL min_from -> Boolean
  test i = checkValid (weaken i :: IntAL min_to)

strengthenValid
  :: ∀ (min_from :: Int) (min_to :: Int)
   . Reflectable min_from Int
  => Reflectable min_to Int
  => Proxy (IntAL min_from)
  -> Proxy (IntAL min_to)
  -> Effect Unit
strengthenValid _ _ = quickCheck' n test
  where
  test :: IntAL min_from -> Boolean
  test i = nothingOrValid (strengthen i :: Maybe (IntAL min_to))


remainderValid :: Int -> Int -> Effect Unit
remainderValid i j = quickCheck' n $ checkValid (remainder i j :: IntAL 0)

gcdValid
  :: ∀ (min_i :: Int) (min_j :: Int)
   . Reflectable min_i Int
  => Reflectable min_j Int
  => Compare 0 min_i LT
  => Compare 0 min_j LT
  => Proxy (IntAL min_i)
  -> Proxy (IntAL min_j)
  -> Effect Unit
gcdValid _ _ = quickCheck' n test
  where
  test :: IntAL min_i -> IntAL min_j -> Boolean
  test i j = checkValid $ gcd i j

lcmValid
  :: ∀ (min_i :: Int) (min_j :: Int) (min_j_plus_one :: Int)
   . Reflectable min_i Int
  => Reflectable min_j Int
  => Add 1 min_j min_j_plus_one
  => Compare 0 min_i LT
  => Compare min_i min_j_plus_one LT
  => Proxy (IntAL min_i)
  -> Proxy (IntAL min_j)
  -> Effect Unit
lcmValid _ _ = quickCheck' n test
  where
  test :: IntAL min_i -> IntAL min_j -> Boolean
  test i j = checkValid $ lcm i j

nothingOrValid
  :: ∀ (min :: Int). Reflectable min Int => Maybe (IntAL min) -> Boolean
nothingOrValid Nothing = true
nothingOrValid (Just i) = checkValid i

n :: Int
n = 1000

main :: Effect Unit
main =
  let
    minusthousand = Proxy :: _ (IntAL (-1000))
    minusone = Proxy :: _ (IntAL (-1))
    zero = Proxy :: _ (IntAL 0)
    one = Proxy :: _ (IntAL 1)
    two = Proxy :: _ (IntAL 2)
    seven = Proxy :: _ (IntAL 7)
    thousand = Proxy :: _ (IntAL 1000)
  in
    do
      fromIntValid minusthousand
      fromIntValid minusone
      fromIntValid zero
      fromIntValid one
      fromIntValid two
      fromIntValid seven
      fromIntValid thousand

      plusValid minusthousand minusthousand
      plusValid minusthousand minusone
      plusValid minusthousand zero
      plusValid minusthousand one
      plusValid minusthousand two
      plusValid minusthousand seven
      plusValid minusthousand thousand
      plusValid minusone minusthousand
      plusValid minusone minusone
      plusValid minusone zero
      plusValid minusone one
      plusValid minusone two
      plusValid minusone seven
      plusValid minusone thousand
      plusValid zero minusthousand
      plusValid zero minusone
      plusValid zero zero
      plusValid zero one
      plusValid zero two
      plusValid zero seven
      plusValid zero thousand
      plusValid one minusthousand
      plusValid one minusone
      plusValid one zero
      plusValid one one
      plusValid one two
      plusValid one seven
      plusValid one thousand
      plusValid two minusthousand
      plusValid two minusone
      plusValid two zero
      plusValid two one
      plusValid two two
      plusValid two seven
      plusValid two thousand
      plusValid seven minusthousand
      plusValid seven minusone
      plusValid seven zero
      plusValid seven one
      plusValid seven two
      plusValid seven seven
      plusValid seven thousand
      plusValid thousand minusthousand
      plusValid thousand minusone
      plusValid thousand zero
      plusValid thousand one
      plusValid thousand two
      plusValid thousand seven
      plusValid thousand thousand

      plusCompatibleWithAdd minusthousand minusthousand
      plusCompatibleWithAdd minusthousand minusone
      plusCompatibleWithAdd minusthousand zero
      plusCompatibleWithAdd minusthousand one
      plusCompatibleWithAdd minusthousand two
      plusCompatibleWithAdd minusthousand seven
      plusCompatibleWithAdd minusthousand thousand
      plusCompatibleWithAdd minusone minusthousand
      plusCompatibleWithAdd minusone minusone
      plusCompatibleWithAdd minusone zero
      plusCompatibleWithAdd minusone one
      plusCompatibleWithAdd minusone two
      plusCompatibleWithAdd minusone seven
      plusCompatibleWithAdd minusone thousand
      plusCompatibleWithAdd zero minusthousand
      plusCompatibleWithAdd zero minusone
      plusCompatibleWithAdd zero zero
      plusCompatibleWithAdd zero one
      plusCompatibleWithAdd zero two
      plusCompatibleWithAdd zero seven
      plusCompatibleWithAdd zero thousand
      plusCompatibleWithAdd one minusthousand
      plusCompatibleWithAdd one minusone
      plusCompatibleWithAdd one zero
      plusCompatibleWithAdd one one
      plusCompatibleWithAdd one two
      plusCompatibleWithAdd one seven
      plusCompatibleWithAdd one thousand
      plusCompatibleWithAdd two minusthousand
      plusCompatibleWithAdd two minusone
      plusCompatibleWithAdd two zero
      plusCompatibleWithAdd two one
      plusCompatibleWithAdd two two
      plusCompatibleWithAdd two seven
      plusCompatibleWithAdd two thousand
      plusCompatibleWithAdd seven minusthousand
      plusCompatibleWithAdd seven minusone
      plusCompatibleWithAdd seven zero
      plusCompatibleWithAdd seven one
      plusCompatibleWithAdd seven two
      plusCompatibleWithAdd seven seven
      plusCompatibleWithAdd seven thousand
      plusCompatibleWithAdd thousand minusthousand
      plusCompatibleWithAdd thousand minusone
      plusCompatibleWithAdd thousand zero
      plusCompatibleWithAdd thousand one
      plusCompatibleWithAdd thousand two
      plusCompatibleWithAdd thousand seven
      plusCompatibleWithAdd thousand thousand

      timesValid zero zero
      timesValid zero one
      timesValid zero two
      timesValid zero seven
      timesValid zero thousand
      timesValid one zero
      timesValid one one
      timesValid one two
      timesValid one seven
      timesValid one thousand
      timesValid two zero
      timesValid two one
      timesValid two two
      timesValid two seven
      timesValid two thousand
      timesValid seven zero
      timesValid seven one
      timesValid seven two
      timesValid seven seven
      timesValid seven thousand
      timesValid thousand zero
      timesValid thousand one
      timesValid thousand two
      timesValid thousand seven
      timesValid thousand thousand

      timesCompatibleWithMul zero zero
      timesCompatibleWithMul zero one
      timesCompatibleWithMul zero two
      timesCompatibleWithMul zero seven
      timesCompatibleWithMul zero thousand
      timesCompatibleWithMul one zero
      timesCompatibleWithMul one one
      timesCompatibleWithMul one two
      timesCompatibleWithMul one seven
      timesCompatibleWithMul one thousand
      timesCompatibleWithMul two zero
      timesCompatibleWithMul two one
      timesCompatibleWithMul two two
      timesCompatibleWithMul two seven
      timesCompatibleWithMul two thousand
      timesCompatibleWithMul seven zero
      timesCompatibleWithMul seven one
      timesCompatibleWithMul seven two
      timesCompatibleWithMul seven seven
      timesCompatibleWithMul seven thousand
      timesCompatibleWithMul thousand zero
      timesCompatibleWithMul thousand one
      timesCompatibleWithMul thousand two
      timesCompatibleWithMul thousand seven
      timesCompatibleWithMul thousand thousand

      weakenValid minusthousand minusthousand
      weakenValid minusone minusthousand
      weakenValid minusone minusone
      weakenValid zero minusthousand
      weakenValid zero minusone
      weakenValid zero zero
      weakenValid one minusthousand
      weakenValid one minusone
      weakenValid one zero
      weakenValid one one
      weakenValid two minusthousand
      weakenValid two minusone
      weakenValid two zero
      weakenValid two one
      weakenValid two two
      weakenValid seven minusthousand
      weakenValid seven minusone
      weakenValid seven zero
      weakenValid seven one
      weakenValid seven two
      weakenValid seven seven
      weakenValid thousand minusthousand
      weakenValid thousand minusone
      weakenValid thousand zero
      weakenValid thousand one
      weakenValid thousand two
      weakenValid thousand seven
      weakenValid thousand thousand

      strengthenValid minusthousand minusthousand
      strengthenValid minusthousand minusone
      strengthenValid minusthousand zero
      strengthenValid minusthousand one
      strengthenValid minusthousand two
      strengthenValid minusthousand seven
      strengthenValid minusthousand thousand
      strengthenValid minusone minusthousand
      strengthenValid minusone minusone
      strengthenValid minusone zero
      strengthenValid minusone one
      strengthenValid minusone two
      strengthenValid minusone seven
      strengthenValid minusone thousand
      strengthenValid zero minusthousand
      strengthenValid zero minusone
      strengthenValid zero zero
      strengthenValid zero one
      strengthenValid zero two
      strengthenValid zero seven
      strengthenValid zero thousand
      strengthenValid one minusthousand
      strengthenValid one minusone
      strengthenValid one zero
      strengthenValid one one
      strengthenValid one two
      strengthenValid one seven
      strengthenValid one thousand
      strengthenValid two minusthousand
      strengthenValid two minusone
      strengthenValid two zero
      strengthenValid two one
      strengthenValid two two
      strengthenValid two seven
      strengthenValid two thousand
      strengthenValid seven minusthousand
      strengthenValid seven minusone
      strengthenValid seven zero
      strengthenValid seven one
      strengthenValid seven two
      strengthenValid seven seven
      strengthenValid seven thousand
      strengthenValid thousand minusthousand
      strengthenValid thousand minusone
      strengthenValid thousand zero
      strengthenValid thousand one
      strengthenValid thousand two
      strengthenValid thousand seven
      strengthenValid thousand thousand

      remainderValid (-1000) (-1000)
      remainderValid (-1000) (-1)
      remainderValid (-1000) 0
      remainderValid (-1000) 1
      remainderValid (-1000) 2
      remainderValid (-1000) 7
      remainderValid (-1000) 1000
      remainderValid (-1) (-1000)
      remainderValid (-1) (-1)
      remainderValid (-1) 0
      remainderValid (-1) 1
      remainderValid (-1) 2
      remainderValid (-1) 7
      remainderValid (-1) 1000
      remainderValid 0 (-1000)
      remainderValid 0 (-1)
      remainderValid 0 0
      remainderValid 0 1
      remainderValid 0 2
      remainderValid 0 7
      remainderValid 0 1000
      remainderValid 1 (-1000)
      remainderValid 1 (-1)
      remainderValid 1 0
      remainderValid 1 1
      remainderValid 1 2
      remainderValid 1 7
      remainderValid 1 1000
      remainderValid 2 (-1000)
      remainderValid 2 (-1)
      remainderValid 2 0
      remainderValid 2 1
      remainderValid 2 2
      remainderValid 2 7
      remainderValid 2 1000
      remainderValid 7 (-1000)
      remainderValid 7 (-1)
      remainderValid 7 0
      remainderValid 7 1
      remainderValid 7 2
      remainderValid 7 7
      remainderValid 7 1000
      remainderValid 1000 (-1000)
      remainderValid 1000 (-1)
      remainderValid 1000 0
      remainderValid 1000 1
      remainderValid 1000 2
      remainderValid 1000 7
      remainderValid 1000 1000

      gcdValid one one
      gcdValid one two
      gcdValid one seven
      gcdValid one thousand
      gcdValid two one
      gcdValid two two
      gcdValid two seven
      gcdValid two thousand
      gcdValid seven one
      gcdValid seven two
      gcdValid seven seven
      gcdValid seven thousand
      gcdValid thousand one
      gcdValid thousand two
      gcdValid thousand seven
      gcdValid thousand thousand

      lcmValid one one
      lcmValid one two
      lcmValid one seven
      lcmValid one thousand
      lcmValid two two
      lcmValid two seven
      lcmValid two thousand
      lcmValid seven seven
      lcmValid seven thousand
      lcmValid thousand thousand
