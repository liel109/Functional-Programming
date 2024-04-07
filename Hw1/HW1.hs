-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW1 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||))

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
uncurry3 f (a,b,c) = f a b c

rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate f c a b = f a b c

lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f b c a = f a b c

-- Generalizations of (.)
(.:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:) f g a b = f . (g a b)

(.:.) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.:.) f g d = f .: (g d)

(.::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::) f g e = f .:. (g e)

(.::.) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.::.) f g x = f .:: (g x)
 
-- -- How can we ever implement such a function!?
-- impossible :: a -> b
-- impossible a = b

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
toBinary n = n `mod` 2 + (toBinary (n `div` 2) * 10)

fromBinary :: Integer -> Integer
fromBinary 0 = 0
fromBinary 1 = 1
fromBinary n = (n `mod` 10) + fromBinary (n `div` 10) * 2

isAbundant :: Integer -> Bool
isAbundant n | n < 0 = False
isAbundant n = divisorsSum 1 > n
    where
        divisorsSum i | i >= n = 0
        divisorsSum i = if n `mod` i == 0 then i + divisorsSum (i+1) else divisorsSum (i+1)

exp10 :: Integer -> Integer
exp10 0 = 1
exp10 x = 10 * exp10 (x-1)

rotateDigits :: Integer -> Integer
rotateDigits n | n >= 0 = (n `div` (exp10 (countDigits n - 1))) + ((n `mod` (exp10 (countDigits n - 1))) * 10)
rotateDigits n = -(((-n) `div` 10) + (((-n) `mod` 10) * (exp10 (countDigits n - 1))))

-- -- ********* --
-- -- Section 3
-- -- ********* --
-- type Generator a = (a -> a, a -> Bool, a)
-- nullGen :: Generator a -> Bool
-- lastGen :: Generator a -> a
-- lengthGen :: Generator a -> Int
-- sumGen :: Generator Integer -> Integer

-- type Predicate a = a -> Bool
-- anyGen :: Predicate a -> Generator a -> Bool
-- allGen :: Predicate a -> Generator a -> Bool
-- noneGen :: Predicate a -> Generator a -> Bool
-- countGen :: Predicate a -> Generator a -> Int

-- -- ********* --
-- -- Section 4
-- -- ********* --
-- isPrime :: Integer -> Bool
-- isSemiprime :: Integer -> Bool
-- goldbachPair :: Integer -> (Integer, Integer)
-- goldbachPair' :: Integer -> (Integer, Integer)

-- -- ***** --
-- -- Bonus
-- -- ***** --
-- isCircularPrime :: Integer -> Bool
-- -- If you choose the implement this function, replace this with the actual implementation
-- isCircularPrime = undefined
