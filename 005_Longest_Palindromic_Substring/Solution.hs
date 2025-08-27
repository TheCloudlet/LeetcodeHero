-- Leetcode 005 Longest Palindromic Substring

module Solution where

import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Solution 1: Expand Around Center
-- Time complexity: O(n^2)
-- Space complexity: O(n) for the array
longestPalindrome1 :: String -> String
longestPalindrome1 str =
  let n = length str
      strArr = listArray (0, n - 1) str
      oddPalindromes = [expandAroundCenter strArr i i | i <- [0 .. n - 1]]
      evenPalindromes = [expandAroundCenter strArr i (i + 1) | i <- [0 .. n - 2]]
      allPalindromes = oddPalindromes ++ evenPalindromes
      (start, len) =
        if null allPalindromes
          then (0, 0)
          else maximumBy (comparing snd) allPalindromes
   in take len (drop start str)

-- Return value is (startPos, length)
expandAroundCenter :: Array Int Char -> Int -> Int -> (Int, Int)
expandAroundCenter arr = go
  where
    (_, maxIndex) = bounds arr
    go l' r'
      | l' < 0 || r' > maxIndex || arr ! l' /= arr ! r' = (l' + 1, r' - l' - 1)
      | otherwise = go (l' - 1) (r' + 1)

main :: IO ()
main = do
  putStrLn "-----------------------------------------------------"
  putStrLn "Testing Longest Palindromic Substring Solution:"
  putStrLn "-----------------------------------------------------"

  -- Run test cases
  testCase "Test Case 1" "babad" ["bab", "aba"] -- multiple valid answers
  testCase "Test Case 2" "cbbd" ["bb"]
  testCase "Test Case 3" "a" ["a"]
  testCase "Test Case 4" "ac" ["a", "c"] -- single chars are palindromes
  testCase "Test Case 5" "racecar" ["racecar"]
  testCase "Test Case 6" "aacabdkacaa" ["aca"]
  testCase "Test Case 7" "" [""] -- edge case: empty string
  testCase "Test Case 8" "aaaaa" ["aaaaa"] -- all same character
  testCase "Test Case 9" "bananas" ["anana"]
  testCase "Test Case 10" "abcdefgfedcba" ["abcdefgfedcba"] -- perfectly symmetric

-- Helper function to run test cases and display results
testCase :: String -> String -> [String] -> IO ()
testCase testName input expectedResults = do
  let result1 = longestPalindrome1 input
      -- result2 = longestPalindrome2 input
      isPassing1 = result1 `elem` expectedResults
      -- isPassing2 = result2 `elem` expectedResults
      status1 = if isPassing1 then "PASS ✓" else "FAIL ✗"
  -- status2 = if isPassing2 then "PASS ✓" else "FAIL ✗"

  putStrLn $ "\n" ++ testName ++ ":"
  putStrLn $ "  Input:    \"" ++ input ++ "\""
  putStrLn $ "  Expected: " ++ showExpected expectedResults
  putStrLn $ "  Solution 1 (Expand Center):"
  putStrLn $ "    Output: \"" ++ result1 ++ "\""
  putStrLn $ "    Status: " ++ status1
  where
    -- putStrLn $ "  Solution 2 (Dynamic Programming):"
    -- putStrLn $ "    Output: \"" ++ result2 ++ "\""
    -- putStrLn $ "    Status: " ++ status2

    showExpected [x] = "\"" ++ x ++ "\""
    showExpected xs = "One of: " ++ show xs
