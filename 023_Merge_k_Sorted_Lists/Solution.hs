-- Leetcode 23. Merge k Sorted Lists

module Solution where

import Data.List (foldl')

-- Merge two sorted lists (descending order)
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x >= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Merge k sorted lists using divide and conquer approach
-- This is O(N log k) where N is total elements and k is number of lists
mergeKLists :: Ord a => [[a]] -> [a]
mergeKLists [] = []
mergeKLists lists = mergePairs lists
  where
    mergePairs [l] = l
    mergePairs ls = mergePairs (mergePairsOnce ls)

    mergePairsOnce [] = []
    mergePairsOnce [l] = [l]
    mergePairsOnce (l1:l2:ls) = merge l1 l2 : mergePairsOnce ls
