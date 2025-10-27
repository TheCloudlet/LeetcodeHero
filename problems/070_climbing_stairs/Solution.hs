-- Leetcode 70. Climbing Stairs

module Solution where

-- Bottom-up DP approach
climbStairs :: Int -> Int
climbStairs 0 = 0
climbStairs 1 = 1
climbStairs n = fibs !! n
  where
    fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)

-- Top-down DP with memoization approach
climbStairsMemo :: Int -> Int
climbStairsMemo n = memo !! n
  where
    -- 'memo' is our cache, an infinite list of all Fibonacci numbers.
    memo = map fib' [0 ..]
    -- 'fib'' is the classic recursive definition.
    fib' 0 = 0
    fib' 1 = 1
    -- The key difference: the recursive step looks into our 'memo' cache!
    fib' k = (memo !! (k - 2)) + (memo !! (k - 1))

main :: IO ()
main = do
  print $ climbStairs 2 -- 2
  print $ climbStairs 3 -- 3
  print $ climbStairs 4 -- 5
  print $ climbStairs 5 -- 8
  print $ climbStairs 6 -- 13
