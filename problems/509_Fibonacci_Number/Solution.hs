-- Leetcode 509. Fibonacci Number

module Solution where

-- Top-down DP with memoization approach
fibMemo :: Int -> Int
fibMemo n = memo !! n where
  memo = map fib' [0 ..]
  fib' 0 = 0
  fib' 1 = 1
  fib' k = (memo !! (k - 2)) + (memo !! (k - 1))

-- Bottom-up DP approach
fib :: Int -> Int
fib n = fibs !! n where
  fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

main :: IO ()
main = do
  print $ fibMemo 0 -- 0
  print $ fibMemo 1 -- 1
  print $ fibMemo 2 -- 1
  print $ fibMemo 3 -- 2
  print $ fibMemo 4 -- 3
  print $ fibMemo 5 -- 5