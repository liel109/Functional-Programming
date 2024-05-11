{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import Text.Read (readMaybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

-- Section 1: Tree Serialization
data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)

serialize :: Tree Int -> [Int]
serialize Empty = [-1]
serialize (Tree l x r) = [x] ++ serialize l ++ serialize r

deserialize :: [Int] -> Tree Int
deserialize = fst . deserialize'

deserialize' :: [Int] -> (Tree Int, [Int])
deserialize' (-1 : rest) = (Empty, rest)
deserialize' (x : rest) =
  let (leftSubtree, restLeft) = deserialize' rest
      (rightSubtree, restRight) = deserialize' restLeft
   in (Tree leftSubtree x rightSubtree, restRight)
deserialize' [] = error "deserialize: empty list"

-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a

infixr 5 :>

smallFilter :: InfiniteList Integer
smallFilter = ifilter (< 5) naturals

sample :: InfiniteList a -> [a]
sample = take 10 . itoList

smallSample :: InfiniteList a -> [a]
smallSample = take 5 . itoList

itoList :: InfiniteList a -> [a]
itoList (x :> xs) = x : itoList xs

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = x :> iiterate f (f x)

irepeat :: a -> InfiniteList a
irepeat x = x :> irepeat x

iprepend :: [a] -> InfiniteList a -> InfiniteList a
iprepend [] xs = xs
iprepend (x : xs) ys = x :> iprepend xs ys

itake :: Integer -> InfiniteList a -> [a]
itake 0 _ = []
itake n (x :> xs) = x : itake (n - 1) xs

idrop :: Integer -> InfiniteList a -> InfiniteList a
idrop 0 xs = xs
idrop n (_ :> xs) = idrop (n - 1) xs

naturals :: InfiniteList Integer
naturals = iiterate (+ 1) 0

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs

ifilter :: (a -> Bool) -> InfiniteList a -> InfiniteList a
ifilter f (x :> xs) = if f x then x :> ifilter f xs else ifilter f xs

ifind :: (a -> Bool) -> InfiniteList a -> a
ifind f (x :> xs) = if f x then x else ifind f xs

iconcat :: InfiniteList [a] -> InfiniteList a
iconcat (x :> xs) = foldr (:>) (iconcat xs) x

integers :: InfiniteList Integer
integers = imap (\x -> if even x then x `div` 2 else (-x) `div` 2) naturals

-- rationals :: InfiniteList Rational

integerSample :: [Integer]
integerSample = itake 1000 integers

-- -- Bonus: same as rationals, but without repeats!
-- rationals' :: InfiniteList Rational

-- -- Section 3: Stack Machine
-- data StackError = DivisionByZero | StackUnderflow {instruction :: String, stackValue :: Maybe Int} deriving (Show, Eq)

-- data RunError = InstructionError StackError | ParseError {line :: String} deriving (Show, Eq)

-- parseAndRun :: String -> Either RunError [Int]
