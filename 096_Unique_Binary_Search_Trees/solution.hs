-- Leetcode 96: Unique Binary Search Trees
--
-- This solution computes the number of unique binary search trees (BSTs)
-- that can be formed with `n` distinct nodes.
-- The recurrence is based on the Catalan number:
--
--   C(0) = 1
--   C(n) = sum_{i=0 to n-1} C(i) * C(n - i - 1)
--
-- The version below is a pure recursive implementation (no memoization).

numTrees :: Int -> Int
numTrees 0 = 1
numTrees n = sum [ numTrees i * numTrees (n - i - 1) | i <- [0 .. n - 1]]

{-
-- Optional: Top-down memoized version using lazy infinite list
-- This avoids recomputation by leveraging Haskell's laziness.
--
-- NOTE:
-- `!!` is list formatting operator
-- [10, 20, 30] !! 1 -- 20

catalan :: [Int]
catalan = 1 : [ sum [ catalan !! i * catalan !! (n - i - 1) | i <- [0 .. n - 1] ] | n <- [1..] ]

numTrees :: Int -> Int
numTrees n = catalan !! n
-}

main :: IO ()
main = mapM_ (print . numTrees) [0..5]
