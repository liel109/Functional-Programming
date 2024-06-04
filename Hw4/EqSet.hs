{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EqSet
  ( EqSet,
    empty,
    EqSet.insert, -- To avoid name clash with Data.List.insert
    member,
    remove,
    elems,
  )
where

import Data.Either
import Data.List
import Data.Maybe

newtype EqSet a = EqSet {getSet :: [a]}

empty :: EqSet a
empty = EqSet []

member :: (Eq a) => a -> EqSet a -> Bool
member x (EqSet xs) = x `elem` xs

insert :: (Eq a) => a -> EqSet a -> EqSet a
insert x (EqSet xs) = if x `elem` xs then EqSet xs else EqSet (x : xs)

remove :: (Eq a) => a -> EqSet a -> EqSet a
remove x (EqSet xs) = EqSet (delete x xs)

elems :: EqSet a -> [a]
elems = getSet

instance (Eq a) => Eq (EqSet a) where
  (EqSet xs) == (EqSet ys) = all (`elem` ys) xs && all (`elem` xs) ys

instance (Show a) => Show (EqSet a) where
  show (EqSet xs) = "{" ++ show' xs ++ "}"
    where
      show' [] = ""
      show' [y] = show y
      show' (y : ys) = show y ++ ", " ++ show' ys

instance (Eq a) => Semigroup (EqSet a) where
  (EqSet xs) <> (EqSet ys) = EqSet (xs `union` ys)

instance (Eq a) => Monoid (EqSet a) where
  mempty = empty
