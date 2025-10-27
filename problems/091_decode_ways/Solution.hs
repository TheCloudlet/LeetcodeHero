-- Leetcode 91. Decode Ways
-- @tag: need-review

module Solution where

import Data.Array

numDecodings :: String -> Int
numDecodings s
  | null s = 0
  | otherwise =
      let n = length s
          sArr = listArray (0, n - 1) s

          -- 1. Define the final memoized array.
          -- The value at each index `i` is defined to be the result of `go i`.
          memo = array (0, n) [(i, go i) | i <- [0 .. n]]

          -- 2. Define the helper function `go`.
          -- Notice it does NOT take `memo` as an argument. It "closes over" it.
          go :: Int -> Int
          go idx
            -- Base case: We've successfully decoded the entire string. 1 way.
            | idx == n = 1
            -- Base case: A subproblem starting with '0' has 0 ways to be decoded.
            | sArr ! idx == '0' = 0
            -- The main recursive step
            | otherwise =
                let -- Path 1: Decode the single digit.
                    -- This looks up `memo ! (idx + 1)`, which triggers a compute of `go (idx + 1)` if needed.
                    singleCount = memo ! (idx + 1)

                    -- Path 2: Try to decode two digits.
                    twoCount =
                      if idx + 1 < n
                        then
                          let c1 = sArr ! idx
                              c2 = sArr ! (idx + 1)
                           in if c1 == '1' || (c1 == '2' && c2 `elem` "123456")
                                -- This looks up `memo ! (idx + 2)`.
                                then memo ! (idx + 2)
                                else 0
                        else 0
                 in singleCount + twoCount
       in -- 3. The answer is the result of the top-level problem, `go 0`.
          -- Accessing `memo ! 0` kicks off the whole chain of lazy computations.
          memo ! 0

-- Helper function to run a single test case
runTest :: String -> Int -> String -> IO ()
runTest input expected testName = do
  let result = numDecodings input
  let status = if result == expected then "PASS" else "FAIL"
  putStrLn $ testName ++ ": " ++ status ++ " (input: \"" ++ input ++ "\", expected: " ++ show expected ++ ", got: " ++ show result ++ ")"

-- Helper function to run all test cases
runAllTests :: IO ()
runAllTests = do
  putStrLn "-----------------------------------------------------"
  putStrLn "Running Decode Ways Tests..."
  putStrLn "-----------------------------------------------------"
  
  -- Basic test cases
  runTest "12" 2 "Test 1 - Basic case"
  runTest "226" 3 "Test 2 - Multiple ways"
  runTest "06" 0 "Test 3 - Leading zero"
  runTest "10" 1 "Test 4 - Valid 10"
  runTest "27" 1 "Test 5 - Invalid 27"
  
  -- Edge cases
  runTest "" 0 "Test 6 - Empty string"
  runTest "0" 0 "Test 7 - Single zero"
  runTest "1" 1 "Test 8 - Single digit"
  runTest "11" 2 "Test 9 - Double ones"
  runTest "111" 3 "Test 10 - Triple ones"
  
  -- Complex cases
  runTest "1111" 5 "Test 11 - Four ones"
  runTest "12120" 3 "Test 12 - With trailing zero"
  runTest "101" 1 "Test 13 - Zero in middle"
  runTest "230" 0 "Test 14 - Invalid sequence"
  
  putStrLn "-----------------------------------------------------"
  putStrLn "Tests completed!"
  putStrLn "-----------------------------------------------------"

-- Main function to run tests
main :: IO ()
main = runAllTests

