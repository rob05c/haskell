module Tree
( insert
, elem
) where

import Prelude hiding (elem)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = singleton x
insert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (insert x left) right
  | x > a = Node a left (insert x right)

elem :: (Ord a) => a -> Tree a -> Bool
elem x EmptyTree = False
elem x (Node a left right)
  | x == a = True
  | x < a = elem x left
  | x > a = elem x right
