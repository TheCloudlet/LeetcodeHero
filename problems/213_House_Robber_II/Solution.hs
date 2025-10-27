-- Leetcode 213. House Robber II

module Solution where

import Data.List (foldl')

-- For circular houses, we can either:
-- 1. Rob houses from index 0 to n-2 (exclude the last house)
-- 2. Rob houses from index 1 to n-1 (exclude the first house
rob2 :: [Int] -> Int
rob2 [] = 0
rob2 [x] = x
rob2 value = max (rob (init value)) (rob (drop 1 value))

-- Copied from leetcode problem 198. House Robber
rob :: [Int] -> Int
rob value = snd $ foldl' step (0, 0) value
  where
    -- prev2: dp[i-2], prev1: dp[i-1]
    step (prev2, prev1) currValue = (prev1, max (prev2 + currValue) prev1)

testHelper :: [Int] -> Int -> IO ()
testHelper input expected =
  if result == expected
    then putStrLn $ "Test passed: " ++ show result
    else putStrLn $ "Test failed: got " ++ show result ++ ", expected " ++ show expected
  where
    result = rob2 input

main :: IO ()
main = do
  putStrLn "---------- Testing rob2: ---------\n"
  testHelper [2, 3, 2] 3
  testHelper [1, 2, 3, 1] 4
  testHelper [1, 2, 3] 3
  testHelper [1, 3, 1] 3
  testHelper [1, 2] 2
  testHelper [2] 2
  testHelper [] 0