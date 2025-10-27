-- Leetcode 206. Reverse Linked List

module Solution where

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
