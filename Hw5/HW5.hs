{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror Deque.hs HW5.hs
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- remove at the end
module HW5 where

import Control.Applicative (liftA2)
import Data.Char (chr, ord, toLower, toUpper)
import Data.Either
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import Data.Set qualified as S
import Deque (Deque)
import Deque qualified as DQ

data FoldMapFunc a m result = FoldMapFunc {agg :: a -> m, finalize :: m -> result}

foldMap' :: (Foldable t, Monoid m) => FoldMapFunc a m result -> t a -> result
foldMap' FoldMapFunc {agg, finalize} = finalize . foldMap agg

-- Section 1: Foldable functions
fmsum :: (Num a) => FoldMapFunc a (Sum a) a
fmsum = FoldMapFunc Sum getSum

fmor :: FoldMapFunc Bool Any Bool
fmor = FoldMapFunc Any getAny

fmfold :: (Monoid a) => FoldMapFunc a a a
fmfold = FoldMapFunc id id

fmelem :: (Eq a) => a -> FoldMapFunc a Any Bool
fmelem x = FoldMapFunc (Any . (== x)) getAny

fmfind :: (a -> Bool) -> FoldMapFunc a (First a) (Maybe a)
fmfind f = FoldMapFunc (\x -> if f x then First (Just x) else First Nothing) getFirst

fmlength :: FoldMapFunc a (Sum Int) Int
fmlength = FoldMapFunc (const (Sum 1)) getSum

fmnull :: FoldMapFunc a All Bool
fmnull = FoldMapFunc (const $ All False) getAll

-- fmmaximum :: (Ord a) => FoldMapFunc a (First a) (Maybe a)
-- fmminimum :: (Ord a) => FoldMapFunc a (Min (Maybe a)) (Maybe a)
-- fmmaxBy :: Ord b => (a -> b) -> FoldMapFunc a _ (Maybe a)
-- fmminBy :: Ord b => (a -> b) -> FoldMapFunc a _ (Maybe a)

fmtoList :: FoldMapFunc a [a] [a]
fmtoList = FoldMapFunc (: []) id

-- Section 2: Deque instances (Don't forget to implement the instances in Deque.hs as well!)
newtype DequeWrapper a = DequeWrapper (Deque a) deriving (Show, Eq)

instance Semigroup (DequeWrapper a) where
  DequeWrapper dq1 <> DequeWrapper dq2 = DequeWrapper (appendDeque dq1 dq2)
    where
      appendDeque dq1' dq2' = case DQ.popl dq2' of
        Nothing -> dq1'
        Just (x, dq2'') -> appendDeque (DQ.pushr x dq1') dq2''

instance Monoid (DequeWrapper a) where
  mempty = DequeWrapper DQ.empty

instance Foldable DequeWrapper where
  foldMap f (DequeWrapper dq) = foldMapD dq
    where
      foldMapD dq' = case DQ.popl dq' of
        Nothing -> mempty
        Just (x, dq'') -> f x <> foldMapD dq''

instance Functor DequeWrapper where
  fmap f (DequeWrapper dq) = DequeWrapper (fmapD dq)
    where
      fmapD dq' = case DQ.popr dq' of
        Nothing -> DQ.empty
        Just (x, dq'') -> DQ.pushr (f x) (fmapD dq'')

instance Applicative DequeWrapper where
  pure x = DequeWrapper (DQ.pushl x DQ.empty)
  liftA2 f (DequeWrapper d1) (DequeWrapper d2) = DequeWrapper (loopD1 d1 d2)
    where
      loopD1 dq1 dq2 = case DQ.popl dq1 of
        Just (x, rest) -> combine (loopD2 x dq2) (loopD1 rest dq2)
        Nothing -> DQ.empty

      loopD2 x1 dq = case DQ.popl dq of
        Just (x2, rest) -> DQ.pushl (f x1 x2) (loopD2 x1 rest)
        Nothing -> DQ.empty

      combine dq1 dq2 = case DQ.popl dq1 of
        Just (x, rest) -> DQ.pushl x (combine rest dq2)
        Nothing -> dq2

instance Monad DequeWrapper where
  return = pure
  (>>=) = flip foldMap

-- Section 3: Calculator and traverse
class (Monad f) => CalculatorError f where
  divideByZero :: f Int
  missingVariable :: String -> f Int

runCalculator :: (CalculatorError f) => Map String Int -> Expr -> f Int
runCalculator vars = go
  where
    go = \case
      Val x -> pure x
      Var x -> maybe (missingVariable x) pure (vars !? x)
      Add x y -> liftA2 (+) (go x) (go y)
      Sub x y -> liftA2 (-) (go x) (go y)
      Mul x y -> liftA2 (*) (go x) (go y)
      Div x y -> do
        y' <- go y
        if y' == 0 then divideByZero else liftA2 div (go x) (pure y')

-- Instances to implement:
instance CalculatorError Maybe where
  divideByZero = Nothing
  missingVariable _ = Nothing

data Err = DivByZero | MissingVar String deriving (Show, Eq)

instance CalculatorError (Either Err) where
  divideByZero = Left DivByZero
  missingVariable = Left . MissingVar

data Defaults
  = Defaults
  -- This replaces the entire division result, not just the denominator!
  { defaultForDivisionByZero :: Int,
    defaultForVariable :: String -> Int
  }

instance CalculatorError (Reader Defaults) where
  divideByZero = Reader $ \Defaults {defaultForDivisionByZero} -> defaultForDivisionByZero
  missingVariable x = Reader $ \Defaults {defaultForVariable} -> defaultForVariable x

-- From the lectures:
newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f r = Reader $ f . runReader r

instance Applicative (Reader r) where
  pure = Reader . const
  liftA2 f ra rb = Reader $ \r -> f (runReader ra r) (runReader rb r)

instance Monad (Reader r) where
  ra >>= f = Reader $ \r -> runReader (f $ runReader ra r) r

data Expr
  = Val Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- Section 4: Hangman
type Score = Int

hangman :: String -> IO Score
hangman word = do
  let uniqueLetters = S.size . S.fromList $ filter isLetter word
  playHangman word [] 0 uniqueLetters True

-- Game loop function
playHangman :: String -> [Char] -> Int -> Int -> Bool -> IO Score
playHangman word guessed tries unique printWord = do
  if printWord
    then do
      putStrLn (displayWord word guessed)
      putStr "Guess a letter: "
    else return ()
  guess <- getChar
  _ <- getChar -- consume the newline character
  if not (isLetter guess)
    then do
      putStrLn ("Invalid letter guess " ++ [guess] ++ "!")
      putStrLn (displayWord word guessed)
      putStr "Try again :"
      playHangman word guessed tries unique False
    else do
      let letter = toLower guess
      if letter `elem` guessed
        then do
          playHangman word guessed tries unique True
        else
          if letter `elem` map toLower word
            then do
              let newGuessed = letter : guessed
              if length newGuessed == unique
                then do
                  putStrLn "Very good, the word is:"
                  putStrLn word
                  return (tries + 1 - unique)
                else
                  playHangman word newGuessed (tries + 1) unique True
            else do
              putStrLn "Wrong guess!"
              putStrLn (displayWord word guessed)
              putStr "Try again :"
              playHangman word guessed (tries + 1) unique False

-- Display the word with guessed letters revealed
displayWord :: String -> [Char] -> String
displayWord word guessed = unwords [[if toLower c `elem` guessed || not (isLetter c) then c else '_' | c <- word]]

isLetter :: Char -> Bool
isLetter c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'