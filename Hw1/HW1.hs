-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW1 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.

import Data.Binary.Get (Decoder (Fail))
import Distribution.Types.ComponentInclude (ComponentInclude (ci_ann_id))
import Prelude (Bool (..), Eq (..), Foldable (length), Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- ********* --

-- Section 1

-- ********* --

const :: a -> b -> a
const a _ = a

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) f g = g . f

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate f c a b = f a b c

lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f b c a = f a b c

-- Generalizations of (.)
(.:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:) f g a b = f . g a b

(.:.) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.:.) f g a = f .: g a

(.::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::) f g e = f .:. g e

(.::.) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.::.) f g x = f .:: g x

-- -- How can we ever implement such a function!?
impossible :: a -> b
impossible _ = undefined

-- ********* --

-- Section 2

-- ********* --

countDigits :: Integer -> Integer
countDigits n | n > 9 = countDigits (n `div` 10) + 1
countDigits n | n < -9 = countDigits ((-n) `div` 10) + 1
countDigits _ = 1

toBinary :: Integer -> Integer
toBinary 0 = 0
toBinary 1 = 1
toBinary n | n < 0 = -toBinary (-n)
toBinary n = n `mod` 2 + (toBinary (n `div` 2) * 10)

fromBinary :: Integer -> Integer
fromBinary 0 = 0
fromBinary 1 = 1
fromBinary n | n < 0 = -fromBinary (-n)
fromBinary n = (n `mod` 10) + fromBinary (n `div` 10) * 2

isAbundant :: Integer -> Bool
isAbundant n | n < 0 = False
isAbundant n = divisorsSum 1 > n
  where
    divisorsSum i | i >= n = 0
    divisorsSum i = if n `mod` i == 0 then i + divisorsSum (i + 1) else divisorsSum (i + 1)

exp10 :: Integer -> Integer
exp10 0 = 1
exp10 x = 10 * exp10 (x - 1)

rotateDigits :: Integer -> Integer
rotateDigits n | n >= 0 = (n `div` (exp10 (countDigits n - 1))) + ((n `mod` (exp10 (countDigits n - 1))) * 10)
rotateDigits n = -(((-n) `div` 10) + (((-n) `mod` 10) * (exp10 (countDigits n - 1))))

-- ********* --

-- Section 3

-- ********* --

type Generator a = (a -> a, a -> Bool, a)

nullGen :: Generator a -> Bool
nullGen (_, condition, seed) = not $ condition seed

lastGen :: Generator a -> a
-- lastGen (f, condition, seed) =
--   let nextGen = (f, condition, f seed)
--    in if not (nullGen (f, condition, seed)) || not (nullGen nextGen) then seed else lastGen nextGen
lastGen (f, condition, seed) =
  let nextGen = (f, condition, f seed)
   in if nullGen (f, condition, seed) then seed else lastGen nextGen

lengthGen :: Generator a -> Int
lengthGen (f, condition, seed) =
  let nextGen = (f, condition, f seed)
   in if nullGen (f, condition, seed) then 0 else 1 + lengthGen nextGen

sumGen :: Generator Integer -> Integer
sumGen (f, condition, seed) =
  let nextElement = f seed
      nextGen = (f, condition, nextElement)
   in if nullGen nextGen then nextElement else nextElement + sumGen nextGen

type Predicate a = a -> Bool

anyGen :: Predicate a -> Generator a -> Bool
anyGen predicate (f, condition, seed) =
  let nextElement = f seed
      nextGen = (f, condition, nextElement)
   in if nullGen nextGen then predicate nextElement else predicate nextElement || anyGen predicate nextGen

-- Check if we need to count the last element generated
allGen :: Predicate a -> Generator a -> Bool
allGen predicate (f, condition, seed) =
  let nextElement = f seed
      nextGen = (f, condition, nextElement)
   in if nullGen nextGen then predicate nextElement else predicate nextElement && allGen predicate nextGen

noneGen :: Predicate a -> Generator a -> Bool
noneGen predicate generator = not $ anyGen predicate generator

countGen :: Predicate a -> Generator a -> Int
countGen predicate (f, condition, seed) =
  let nextElement = f seed
      nextGen = (f, condition, nextElement)
      countPredicate = if predicate nextElement then 1 else 0
   in if nullGen nextGen then countPredicate else countPredicate + countGen predicate nextGen

-- -- ********* --
-- -- Section 4
-- -- ********* --
isPrime :: Integer -> Bool
isPrime n | n < 2 = False
isPrime n =
  let isDivisor i | i > n `div` 2 = True
      isDivisor i = (not (n `mod` i == 0) && isDivisor (i + 1))
   in isDivisor 2

isSemiprime :: Integer -> Bool
isSemiprime n =
  let countDivisors i | i > n `div` 2 = 0
      countDivisors i = if n `mod` i == 0 then 1 + countDivisors (i + 1) else countDivisors (i + 1)
   in countDivisors 2 == 2

goldbachPair :: Integer -> (Integer, Integer)
goldbachPair n =
  let findPrimes i = if isPrime i && isPrime (n - i) then (i, n - i) else findPrimes (i + 1)
   in findPrimes 2

goldbachPair' :: Integer -> (Integer, Integer)
goldbachPair' n =
  let isGoldbachPair i = isPrime i && isPrime (n - i)
      findPrimes i (a, b) | i > n `div` 2 = (a, b)
      findPrimes i (a, b) = if isGoldbachPair i then findPrimes (i + 1) (maxMultiplication (a, b) (i, n - i)) else findPrimes (i + 1) (a, b)
   in flipPair $ findPrimes 2 (0, 0)

maxMultiplication :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
maxMultiplication (a, b) (c, d) = if (a * b) > (c * d) then (a, b) else (c, d)

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

-- ***** --

-- Bonus

-- ***** --

isCircularPrime :: Integer -> Bool
-- If you choose the implement this function, replace this with the actual implementation
isCircularPrime n | n < 2 = False
isCircularPrime n = isCircularPrime' n 1 False

isCircularPrime' :: Integer -> Integer -> Bool -> Bool
isCircularPrime' n i _ | i > countDigits n = True
isCircularPrime' n i booli | booli = isPrime n && isCircularPrime' (10 * rotateDigits n) (i + 1) False
isCircularPrime' n i _ = isPrime n && if countDigits n > countDigits (rotateDigits n) then isCircularPrime' (rotateDigits n) (i + 1) True else isCircularPrime' (rotateDigits n) (i + 1) False
