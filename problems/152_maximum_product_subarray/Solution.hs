-- Leetcode 152. Maximum Product Subarray

module Solution where

import Data.List (foldl')

-- I wrote the following
maxProduct :: [Int] -> Int
maxProduct [] = 0
maxProduct (x : xs) = go xs x x x
  where
    go [] globalMax _ _ = globalMax
    go (n : ns) globalMax localMax localMin =
      go ns newGlobalMax newLocalMax newLocalMin
      where
        candidate = [n, n * localMax, n * localMin]
        newLocalMax = maximum candidate
        newLocalMin = minimum candidate
        newGlobalMax = max globalMax newLocalMax

-- Preferred by Haskellers
maxProduct' :: [Int] -> Int
maxProduct' [] = 0
maxProduct' (x : xs) =
  let (globalMax, _, _) = foldl' go (x, x, x) xs
   in globalMax
  where
    -- The folding function `go` has the correct signature:
    -- accumulator -> element -> new_accumulator
    -- (global, local_max, local_min) -> n -> (new_global, new_local_max, new_local_min)
    go :: (Int, Int, Int) -> Int -> (Int, Int, Int)
    go (gMax, lMax, lMin) n = (newGMax, newLMax, newLMin)
      where
        candidates = [n, n * lMax, n * lMin]
        newLMax = maximum candidates
        newLMin = minimum candidates
        newGMax = max gMax newLMax
