{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW4.hs EqSet.hs EqMap.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Char (chr, ord)
import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqMap (EqMap)
import EqMap qualified
import EqSet (EqSet)
import EqSet qualified

-- Section 2: Serialization
class Serializable a where
  serialize :: a -> [Int]
  deserialize :: [Int] -> a

instance Serializable Int where
  serialize a = [a]
  deserialize [a] = a
  deserialize _ = error "Invalid"

instance Serializable Bool where
  serialize True = [1]
  serialize False = [0]
  deserialize [1] = True
  deserialize [0] = False
  deserialize _ = error "Invalid"

instance Serializable Char where
  serialize a = [ord a]
  deserialize [a] = chr a
  deserialize _ = error "Invalid"

instance (Serializable a) => Serializable (Maybe a) where
  serialize Nothing = [0]
  serialize (Just a) = 1 : serialize a
  deserialize [0] = Nothing
  deserialize (1 : a) = Just (deserialize a)
  deserialize _ = error "Invalid"

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (a, b) = serialize a ++ serialize b
  deserialize xs =
    let (aSerialized, bSerialized) = splitAt (length (serialize (undefined :: a))) xs
     in (deserialize aSerialized, deserialize bSerialized)

instance (Serializable a, Serializable b) => Serializable (Either a b) where
  serialize (Left a) = 0 : serialize a
  serialize (Right b) = 1 : serialize b
  deserialize (0 : a) = Left (deserialize a)
  deserialize (1 : b) = Right (deserialize b)
  deserialize _ = error "Invalid"

instance (Serializable a) => Serializable [a] where
  serialize [] = [0]
  serialize (x : xs) = 1 : serialize x ++ serialize xs
  deserialize [0] = []
  deserialize (1 : x) =
    let (a, b) = splitAt (length (serialize (undefined :: a))) x
     in deserialize a : deserialize b
  deserialize _ = error "Invalid"

instance (Serializable a, Eq a) => Serializable (EqSet a) where
  serialize = serialize . EqSet.elems
  deserialize xs = foldr EqSet.insert EqSet.empty (deserialize xs :: [a])

instance (Serializable k, Eq k, Serializable v) => Serializable (EqMap k v) where
  serialize = serialize . EqMap.assocs
  deserialize xs = foldr (uncurry EqMap.insert) EqMap.empty (deserialize xs :: [(k, v)])

-- Section 3: Metric
infinity :: Double
infinity = 1 / 0

class (Eq a) => Metric a where
  distance :: a -> a -> Double

instance Metric Double where
  distance x y = abs (x - y)

instance Metric Int where
  distance x y = abs (fromIntegral x - fromIntegral y)

instance Metric Char where
  distance x y = distance (ord x) (ord y)

-- Euclidean distance
instance (Metric a, Metric b) => Metric (a, b) where
  distance (a1, b1) (a2, b2) = sqrt ((distance a1 a2) ** 2 + (distance b1 b2) ** 2)

data ManhattanTuple a b = ManhattanTuple a b deriving (Eq)

instance (Metric a, Metric b) => Metric (ManhattanTuple a b) where
  distance (ManhattanTuple a1 b1) (ManhattanTuple a2 b2) = distance a1 a2 + distance b1 b2

-- Just and Nothing have distance of infinity.
-- Two Justs measure the distance between the two values.
instance (Metric a) => Metric (Maybe a) where
  distance (Just a) (Just b) = distance a b
  distance _ _ = infinity

-- Left and Right have a distance of infinity.
-- Same constructores measure the distance between the two values.
instance (Metric a, Metric b) => Metric (Either a b) where
  distance (Left a) (Left b) = distance a b
  distance (Right a) (Right b) = distance a b
  distance _ _ = infinity

-- Lists of different sizes have distance of infinity.
-- Euclidean distance.
instance (Metric a) => Metric [a] where
  distance xs ys
    | length xs /= length ys = infinity
    | otherwise = sqrt (sum (zipWith (\x y -> distance x y ** 2) xs ys))

newtype ManhattanList a = ManhattanList [a] deriving (Eq)

instance (Metric a) => Metric (ManhattanList a) where
  distance (ManhattanList xs) (ManhattanList ys)
    | length xs /= length ys = infinity
    | otherwise = sum (zipWith distance xs ys)

-- Returns the element with the shortest distance to the input.
-- If there are no numbers whose distance is less than infinity, return Nothing.
closest :: (Metric a) => a -> [a] -> Maybe a
closest = closestOn id

-- Similar to the above, but uses a function move the element
-- to another metric space.
closestOn :: (Metric b) => (a -> b) -> a -> [a] -> Maybe a
closestOn f a xs = closestOn' f a xs infinity a

closestOn' :: (Metric b) => (a -> b) -> a -> [a] -> Double -> a -> Maybe a
closestOn' f a (x : xs) d c = if distance (f a) (f x) < d then closestOn' f a xs (distance (f a) (f x)) x else closestOn' f a xs d c
closestOn' _ _ [] d c = if d == infinity then Nothing else Just c

-- Will not swap elements whose distance is less than d, even if their
-- order implies they should be swapped.
metricBubbleSort :: (Metric a, Ord a) => Double -> [a] -> [a]
metricBubbleSort = metricBubbleSortOn id

-- Similar to the above, but uses a function to extract the value used for sorting.
metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]
metricBubbleSortOn f d xs = applyN (length xs) (bubbleOn f d) xs

bubbleOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]
bubbleOn _ _ [] = []
bubbleOn _ _ [x] = [x]
bubbleOn f d (x : y : xs)
  | f x > f y && distance (f x) (f y) >= d = y : bubbleOn f d (x : xs)
  | otherwise = x : bubbleOn f d (y : xs)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ = id
applyN n f = f . applyN (n - 1) f

-- Bonus (10 points).
-- clusters :: (Metric a) => [a] -> [[a]]
-- clusters [] = []
-- clusters (x : xs) =
--   let (cluster, rest) = partition ((< infinity) . distance x) xs
--    in (x : cluster) : clusters rest