-- Leetcode 21. Merge Two Sorted Lists

module Solution where

mergeTwoLists :: (Ord a) => [a] -> [a] -> [a]
mergeTwoLists [] ys = ys
mergeTwoLists xs [] = xs
mergeTwoLists (x:xs) (y:ys)
  | x < y     = x : mergeTwoLists xs (y:ys)
  | otherwise = y : mergeTwoLists (x:xs) ys
