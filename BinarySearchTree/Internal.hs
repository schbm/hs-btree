{-# LANGUAGE InstanceSigs #-}

module BinarySearchTree.Internal where

import Test.QuickCheck
import qualified Data.List
import qualified Data.Type.Bool as BST


data T a = Leaf | Node (T a) a (T a)
  deriving (Show)

valid :: Ord a => T a -> Bool
valid Leaf = True
valid (Node ltt val rtt)
  | validSub ltt val (<) && validSub rtt val (>) = True
  | otherwise = False
  where
    validSub :: Ord a => T a -> a -> (a -> a -> Bool) -> Bool
    validSub Leaf _ _ = True
    validSub (Node ltt' val' rtt') parentVal comparator =
      val' `comparator` parentVal && validSub ltt' parentVal comparator && validSub rtt' parentVal comparator

empty :: T a
empty = Leaf

insert :: Ord a => a -> T a -> T a
insert e Leaf = Node Leaf e Leaf
insert e (Node ltt p gtt) | e < p = Node (insert e ltt) p gtt
                          | e > p = Node ltt p (insert e gtt)
                          | otherwise = Node ltt p gtt

fromList :: Ord a => [a] -> T a
fromList = foldr insert empty

-- returns valid BSTs
prop_valid_empty :: Bool
prop_valid_empty = valid (empty :: T Int)


prop_valid_insert :: Ord a => a -> T a -> Bool
prop_valid_insert e t = valid (insert e t)

-- >>> :t prop_valid_insert
-- prop_valid_insert :: Ord a => a -> T a -> Bool

-- >>> :t prop_valid_insert @Int
-- prop_valid_insert @Int :: Int -> T Int -> Bool

-- prop> prop_valid_insert @Int
-- +++ OK, passed 100 tests.

-- prop> prop_valid_insert
-- +++ OK, passed 100 tests.


-- only returns valid BSTs
prop_valid_fromList :: Ord a => [a] -> Bool
prop_valid_fromList xs = valid $ fromList xs

-- prop> prop_valid_fromList @Int
-- +++ OK, passed 100 tests.

toList :: T a -> [a]
toList Leaf = []
toList (Node lt x rt) = toList lt ++ [x] ++ toList rt

member :: Ord a => a -> T a -> Bool
member x Leaf = False
member x (Node l y r) | x == y = True
                      | x < y = member x l
                      | otherwise = member x r

-- member of an empty BST.
prop_member_empty :: Ord a => a -> Bool
prop_member_empty e = not (member e empty)

-- prop> prop_member_empty @Int
-- +++ OK, passed 100 tests.

-- Inserting an element into a BST makes it a member of the BST.
prop_member_insert :: Ord a => a -> T a -> Bool
prop_member_insert e t = member e (insert e t)

-- prop> prop_member_insert @Int
-- +++ OK, passed 100 tests.

prop_member_fromList :: Ord a => [a] -> Bool
prop_member_fromList xs = and [member x (fromList xs) | x <- xs]

-- prop> prop_member_fromList @Int
-- +++ OK, passed 100 tests.

unionHelp :: Ord a => T a -> T a -> T a
unionHelp Leaf t2 = t2
unionHelp t1 Leaf = t1
unionHelp (Node l1 v1 r1) t2 = insert v1 (unionHelp (unionHelp l1 t2) r1)

union :: Ord a => T a -> T a -> T a
union Leaf Leaf = Leaf
union t1 Leaf = t1
union Leaf t2 = t2
union t1 t2 = fromList (merge (toList t1) (toList t2))
              where
                merge :: Ord a => [a] -> [a] -> [a]
                merge [] yl = yl
                merge xl [] = xl
                merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                                    | x == y = x : y : merge xs ys
                                    | otherwise = y : merge (x:xs) ys

-- +++ OK, passed 100 tests.
prop_valid_union :: Ord a => T a -> T a -> Bool
prop_valid_union ta tb = valid $ union ta tb

-- prop> prop_valid_union @Int
-- +++ OK, passed 100 tests.

prop_member_union :: Ord a => a -> T a -> T a -> Bool
prop_member_union e ta tb =
   (e `member` ta || e `member` tb) == (e `member` (ta `union` tb))

-- prop> prop_member_union @Int
-- +++ OK, passed 100 tests.

delete :: Ord a => a -> T a -> T a
delete x n = fromList (deleteFromList x (toList n))
            where
              deleteFromList :: Ord a => a -> [a] -> [a]
              deleteFromList _ [] = []
              deleteFromList x (y:ys) | x == y = deleteFromList x ys
                                      | otherwise = y : deleteFromList x ys

prop_valid_delete :: Ord a => a -> T a -> Bool
prop_valid_delete x t = valid $ delete x t

-- prop> prop_valid_delete
-- +++ OK, passed 100 tests.

prop_member_delete :: Ord a => a -> T a -> Bool
prop_member_delete x t = not (member x (delete x t))

-- prop> prop_member_delete @Int
-- +++ OK, passed 100 tests.

instance Ord a => Eq (T a) where
  (==) :: Ord a => T a -> T a -> Bool
  Leaf == Leaf = True
  ta == tb = toList ta == toList tb

instance Ord a => Semigroup (T a) where
  (<>) :: Ord a => T a -> T a -> T a
  (<>) = union

instance Ord a => Monoid (T a) where
  mempty :: Ord a => T a
  mempty = empty
