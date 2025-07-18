module LinkedList
  ( LinkedList(..)
  , fromStdList
  , toStdList
  ) where

data LinkedList a = LNil | LCons a (LinkedList a) deriving (Show, eq)

instance Foldable LinkedList where
  foldr _ z LNil = z
  foldr f z (LCons x xs) = f x (foldr f z xs)

fromStdList :: [a] -> LinkedList a
fromStdList = foldr LCons a LNil

toStdList :: LinkedList a -> [a]
toStdList = foldr (:) []
