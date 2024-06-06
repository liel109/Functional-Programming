{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EqMap
  ( EqMap,
    CombiningMap (..),
    empty,
    EqMap.insert, -- To avoid name clash with Data.List.insert
    member,
    remove,
    EqMap.lookup, -- To avoid name clash with Prelude.lookup
    assocs,
  )
where

import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqSet (EqSet)
import EqSet qualified

data EqMap k v = EqMap {getMap :: EqSet (Arg k v)}

empty :: EqMap k v
empty = EqMap EqSet.empty

member :: (Eq k) => k -> EqMap k v -> Bool
member key (EqMap m) = EqSet.member (Arg key undefined) m

insert :: (Eq k) => k -> v -> EqMap k v -> EqMap k v
insert key value m = EqMap $ EqSet.insert (Arg key value) (getMap (remove key m))

remove :: (Eq k) => k -> EqMap k v -> EqMap k v
remove key (EqMap m) = EqMap (EqSet.remove (Arg key undefined) m)

lookup :: (Eq k) => k -> EqMap k v -> Maybe v
lookup k (EqMap m) = go (EqSet.elems m)
  where
    go [] = Nothing
    go (Arg k' v : xs)
      | k == k' = Just v
      | otherwise = go xs

assocs :: EqMap k v -> [(k, v)]
assocs (EqMap m) = go (EqSet.elems m)
  where
    go [] = []
    go (Arg k v : xs) = (k, v) : go xs

instance (Eq k, Eq v) => Eq (EqMap k v) where
  m1 == m2 = all (`aux` listm2) listm1 && all (`aux` listm1) listm2
    where
      listm1 = EqSet.elems (getMap m1)
      listm2 = EqSet.elems (getMap m2)
      aux _ [] = False
      aux (Arg k1 v1) (Arg k2 v2 : rest) = (k1 == k2 && v1 == v2) || aux (Arg k1 v1) rest

instance (Show k, Show v) => Show (EqMap k v) where
  show :: (Show k, Show v) => EqMap k v -> String
  show (EqMap m) = "{" ++ show' (EqSet.elems m) ++ "}"
    where
      show' [] = ""
      show' [Arg k v] = show k ++ " -> " ++ show v
      show' (Arg k v : xs) = show k ++ " -> " ++ show v ++ ", " ++ show' xs

instance (Eq k) => Semigroup (EqMap k v) where
  m1 <> m2 = EqMap (getMap m2 <> getMap m1)

instance (Eq k) => Monoid (EqMap k v) where
  mempty = empty

newtype CombiningMap k v = CombiningMap {getCombiningMap :: EqMap k v}

instance (Eq k, Semigroup v) => Semigroup (CombiningMap k v) where
  cm1 <> cm2 =
    CombiningMap $ EqMap $ foldr insertOrCombine (getMap $ getCombiningMap cm1) (EqSet.elems (getMap $ getCombiningMap cm2))
    where
      insertOrCombine (Arg k v) acc = getMap . insertWithCombine k v . EqMap $ acc
      insertWithCombine k v m =
        case EqMap.lookup k m of
          Just v' -> EqMap.insert k (v' <> v) m
          Nothing -> EqMap.insert k v m

instance (Eq k, Semigroup v) => Monoid (CombiningMap k v) where
  mempty = CombiningMap empty
