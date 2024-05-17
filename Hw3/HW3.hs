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
import Data.Text.Array (run)
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

-- Section 3: Stack Machine
data StackError = DivisionByZero | StackUnderflow {instruction :: String, stackValue :: Maybe Int} deriving (Show, Eq)

data RunError = InstructionError StackError | ParseError {line :: String} deriving (Show, Eq)

maybeMapList :: (a -> Maybe b) -> [a] -> Maybe [b]
maybeMapList _ [] = Just []
maybeMapList f (x : xs) = case f x of
  Just y -> case maybeMapList f xs of
    Just ys -> Just (y : ys)
    Nothing -> Nothing
  Nothing -> Nothing

foldlEither :: (b -> a -> Either e b) -> b -> [a] -> Either e b
foldlEither _ acc [] = Right acc
foldlEither f acc (x : xs) = case f acc x of
  Left e -> Left e
  Right y -> foldlEither f y xs

data Instruction
  = PUSH Int
  | POP
  | ADD
  | SUB
  | MUL
  | DIV
  | DUP
  | SWAP
  deriving (Show, Eq)

parseInstruction :: String -> Maybe Instruction
parseInstruction str = case words str of
  ["PUSH", n] -> case readMaybe n of
    Just x -> Just (PUSH x)
    Nothing -> Nothing
  ["POP"] -> Just POP
  ["ADD"] -> Just ADD
  ["SUB"] -> Just SUB
  ["MUL"] -> Just MUL
  ["DIV"] -> Just DIV
  ["DUP"] -> Just DUP
  ["SWAP"] -> Just SWAP
  _ -> Nothing

runInstruction :: [Int] -> Instruction -> Either RunError [Int]
runInstruction stack instr = case runInstruction' stack instr of
  Left err -> Left (InstructionError err)
  Right newStack -> Right newStack

runInstruction' :: [Int] -> Instruction -> Either StackError [Int]
runInstruction' stack = \case
  PUSH n -> runPush n stack
  POP -> runPop stack
  ADD -> runOp (+) stack "ADD"
  SUB -> runOp (-) stack "SUB"
  MUL -> runOp (*) stack "MUL"
  DIV -> runDiv stack
  DUP -> runDup stack
  SWAP -> runSwap stack

runPush :: Int -> [Int] -> Either StackError [Int]
runPush n stack = Right (n : stack)

runPop :: [Int] -> Either StackError [Int]
runPop [] = Left (StackUnderflow "POP" Nothing)
runPop (_ : stack) = Right stack

runSwap :: [Int] -> Either StackError [Int]
runSwap [] = Left (StackUnderflow "SWAP" Nothing)
runSwap [x] = Left (StackUnderflow "SWAP" (Just x))
runSwap (x : y : stack) = Right (y : x : stack)

runDup :: [Int] -> Either StackError [Int]
runDup [] = Left (StackUnderflow "DUP" Nothing)
runDup (x : stack) = Right (x : x : stack)

runOp :: (Int -> Int -> Int) -> [Int] -> String -> Either StackError [Int]
runOp _ [] op = Left (StackUnderflow op Nothing)
runOp _ [x] op = Left (StackUnderflow op (Just x))
runOp f (x : y : stack) _ = Right (f x y : stack)

runDiv :: [Int] -> Either StackError [Int]
runDiv [] = Left (StackUnderflow "DIV" Nothing)
runDiv [x] = Left (StackUnderflow "DIV" (Just x))
runDiv (x : y : stack)
  | y == 0 = Left DivisionByZero
  | otherwise = Right (div x y : stack)

parseAndRun :: String -> Either RunError [Int]
parseAndRun str = parseAndRunLines (lines str) []

parseAndRunLines :: [String] -> [Instruction] -> Either RunError [Int]
parseAndRunLines [] acc = foldlEither runInstruction [] acc
parseAndRunLines (line : lines') acc = case parseInstruction line of
  Just instr -> parseAndRunLines lines' (acc ++ [instr])
  Nothing -> Left (ParseError line)
