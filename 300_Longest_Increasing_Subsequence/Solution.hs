-- Leetcode 300. Longest Increasing Subsequence

module Solution where

import Data.Array
import Text.Printf (printf)

lengthOfLIS :: [Int] -> Int
lengthOfLIS nums
  | null nums = 0
  | otherwise =
      let n = length nums
          bounds = (0, n - 1)
          numsArr = listArray bounds nums
          dp = listArray bounds [calculateLISAt idx | idx <- [0 .. n - 1]]

          -- Calculates the LIS length ending at a specific index.
          calculateLISAt :: Int -> Int
          calculateLISAt currentIdx = 1 + findMaxLISBefore currentIdx
            where
              findMaxLISBefore :: Int -> Int
              findMaxLISBefore currentIdx =
                let validPrevLen =
                      [ dp ! prevIdx
                      | prevIdx <- [0 .. currentIdx - 1],
                        numsArr ! currentIdx > numsArr ! prevIdx
                      ]
                 in foldr max 0 validPrevLen
       in maximum (elems dp)

-- =============================================================================
-- TESTING FRAMEWORK
-- =============================================================================
-- The following code provides a reusable testing template for LeetCode
-- problems. This framework can be adapted for any problem by:
--   1. Changing the TestCase type parameters (input/output types)
--   2. Updating the testCases list with your specific test cases
--   3. Replacing 'lengthOfLIS' with your function name in main
--
-- Features:
--   - Colored output (green for PASS, red for FAIL)
--   - Detailed failure reporting with input/expected/actual
--   - Easy to add new test cases
--   - Type-safe test framework using Haskell's type system
-- =============================================================================

data TestCase a b = TestCase
  { name :: String, -- A descriptive name for the test
    input :: a, -- The input to the function
    expected :: b -- The expected output
  }

-- Test cases for the current problem (Longest Increasing Subsequence).
-- To adapt this template for other problems:
--   1. Change the type signature: TestCase [InputType] OutputType
--   2. Update the test cases below with your problem's examples
--   3. Add edge cases specific to your problem
testCases :: [TestCase [Int] Int]
testCases =
  [ TestCase "Example 1" [10, 9, 2, 5, 3, 7, 101, 18] 4,
    TestCase "Example 2" [0, 1, 0, 3, 2, 3] 4,
    TestCase "Example 3" [7, 7, 7, 7, 7, 7, 7] 1,
    TestCase "Empty List" [] 0,
    TestCase "Single Element" [42] 1,
    TestCase "Already Sorted" [1, 2, 3, 4, 5] 5,
    TestCase "Reverse Sorted" [5, 4, 3, 2, 1] 1,
    TestCase "Complex Case" [3, 5, 6, 2, 5, 4, 19, 5, 6, 7, 12] 6
  ]

-- Helper function to run a single test case.
-- The type constraints ensure that the input/output types can be
-- printed (Show) and the output type can be compared for correctness (Eq).
runTest :: (Show a, Show b, Eq b) => (a -> b) -> TestCase a b -> IO ()
runTest func tc = do
  let actual = func (input tc)
  if actual == expected tc
    then printf "\x1b[32m[PASS]\x1b[0m %s\n" (name tc) -- Green for PASS
    else do
      printf "\x1b[31m[FAIL]\x1b[0m %s\n" (name tc) -- Red for FAIL
      printf "       Input:    %s\n" (show $ input tc)
      printf "       Expected: %s\n" (show $ expected tc)
      printf "       Actual:   %s\n" (show actual)

-- Main entry point.
-- This function iterates through all test cases and runs them.
main :: IO ()
main = do
  putStrLn "-----------------------------------------"
  putStrLn "Running tests for lengthOfLIS..."
  putStrLn "-----------------------------------------"
  -- mapM_ applies the runTest function to each element of testCases.
  -- The function to test, `lengthOfLIS`, is passed as the first argument.
  mapM_ (runTest lengthOfLIS) testCases
  putStrLn "-----------------------------------------"
  putStrLn "Tests complete."