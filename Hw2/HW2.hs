{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- Section 1.1: Basic Maybes
concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap _ Nothing = Nothing
concatMaybeMap f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing = x
maybe _ f (Just y) = f y

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr aux []
  where
    aux (Just y) acc = y : acc
    aux Nothing acc = acc

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f

-- Section 1.2 Basic Eithers

concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap f x = case x of
  Left e -> Left e
  Right y -> f y

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g x = case x of
  Left y -> f y
  Right y -> g y

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

catEithers :: [Either e a] -> Either e [a]
catEithers = foldr aux (Right [])
  where
    aux (Left e) _ = Left e
    aux (Right y) acc = case acc of
      Left e -> Left e
      Right ys -> Right (y : ys)

mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither f = catEithers . map f

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr aux ([], [])
  where
    aux (Left a) (as, bs) = (a : as, bs)
    aux (Right b) (as, bs) = (as, b : bs)


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe x = case x of
  Left _ -> Nothing
  Right y -> Just y

-- Section 2: Lists
take :: Int -> [a] -> [a]
take n x = case x of
  [] -> []
  y : ys -> if n <= 0 then [] else y : take (n - 1) ys

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x : xs) = if f x then x : takeWhile f xs else []

drop :: Int -> [a] -> [a]
drop n x = case x of
  [] -> []
  y : ys -> if n <= 0 then y : ys else drop (n - 1) ys

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x : xs) = if f x then dropWhile f xs else x : xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n x = if n <= 0 then x else rotate (n - 1) (reverse (lotate 1 (reverse x)))

lotate :: Int -> [a] -> [a]
lotate _ [] = []
lotate n (x : xs) = if n <= 0 then x : xs else lotate (n - 1) (xs ++ [x])

type Generator a = (a -> a, a -> Bool, a)

fromGenerator :: Generator a -> [a]
fromGenerator (f, p, x) = if p x then f x : fromGenerator (f, p, f x) else []

replicate :: Int -> a -> [a]
replicate n x = if n <= 0 then [] else x : replicate (n - 1) x

inits :: [a] -> [[a]]
inits = foldr aux [[]]
  where
    aux x acc = [] : map (x :) acc

head :: [a] -> a
head [] = error "empty list"
head (x : _) = x

tails :: [a] -> [[a]]
tails = foldr aux [[]]
  where
    aux x acc = (x : head acc) : acc

-- -- Section 3: zips and products
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
zipFill _ _ [] [] = []
zipFill a b [] (y : ys) = (a, y) : zipFill a b [] ys
zipFill a b (x : xs) [] = (x, b) : zipFill a b xs []
zipFill a b (x : xs) (y : ys) = (x, y) : zipFill a b xs ys

data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)

zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
zipFail [] [] = Right []
zipFail [] _ = Left ErrorFirst
zipFail _ [] = Left ErrorSecond
zipFail (x : xs) (y : ys) = case zipFail xs ys of
  Left e -> Left e
  Right zs -> Right ((x, y) : zs)


unzip :: [(a, b)] -> ([a], [b])
unzip = foldr aux ([], [])
  where
    aux (x, y) (xs, ys) = (x : xs, y : ys)

-- Section 4: Knight travels
-- Position (0, 0) is the top-left corner.
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)

data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)

-- Utility to get all knight moves. Don't worry about the implementation of this.
allKnightMoves :: [KnightMove]
allKnightMoves = [minBound .. maxBound]

data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)

isValid :: Board -> KnightPos -> KnightMove -> Bool
isValid (Board w h) (KnightPos x y) move = n_x >= 0 && n_y >= 0 && n_x < w && n_y < h
    where (KnightPos n_x n_y) = moveKnight (KnightPos x y) move

listValidMoves :: Board -> KnightPos -> [(KnightMove, KnightPos)]
listValidMoves board pos = [(move, moveKnight pos move) | move <- allKnightMoves, isValid board pos move]

listToMaybe :: [Maybe a] -> Maybe a
listToMaybe = foldr aux Nothing
    where aux x acc = maybe acc Just x

tour :: Board -> KnightPos -> Maybe [KnightMove]
tour board start = findATour board start [start]

findATour :: Board -> KnightPos -> [KnightPos] -> Maybe [KnightMove]
findATour board pos posHistory = if length posHistory == totalSquares then Just [] else listToMaybe $ concatMap evaluateMove (listValidMoves board pos)
    where 
        totalSquares = width board * height board
        evaluateMove (move, newPos) = if elem newPos posHistory then [Nothing] else 
          case findATour board newPos (newPos : posHistory) of
                Just moves -> [Just (move : moves)]
                Nothing -> [Nothing]

newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)

moveKnight :: KnightPos -> KnightMove -> KnightPos
moveKnight (KnightPos x y) move = case move of
  TopLeft -> KnightPos (x - 2) (y - 1)
  TopRight -> KnightPos (x + 2) (y - 1)
  RightTop -> KnightPos (x + 1) (y - 2)
  RightBottom -> KnightPos (x + 1) (y + 2)
  BottomRight -> KnightPos (x + 2) (y + 1)
  BottomLeft -> KnightPos (x - 2) (y + 1)
  LeftBottom -> KnightPos (x - 1) (y + 2)
  LeftTop -> KnightPos (x - 1) (y - 2)

translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate _ [] = []
translate pos (x : xs) = moveKnight pos x : translate (moveKnight pos x) xs

findMove :: KnightPos -> KnightPos -> Either InvalidPosition KnightMove
findMove pos1 pos2 = case filter (\x -> moveKnight pos1 x == pos2) allKnightMoves of
  [] -> Left (InvalidPosition pos2)
  (move:_) -> Right move

translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]
translate' [] = Right []
translate' [_] = Right []
translate' (x : y : xs) = case findMove x y of
  Left e -> Left e
  Right move -> case translate' (y : xs) of
    Left e -> Left e
    Right moves -> Right (move : moves)

-- -- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark = undefined