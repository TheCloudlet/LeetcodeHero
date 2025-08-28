-- Leetcode 005 Longest Palindromic Substring

module Solution where

import Data.Array
import Data.List (find, maximumBy)
import qualified Data.Map as Map
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
      result2 = longestPalindrome2 input
      isPassing1 = result1 `elem` expectedResults
      isPassing2 = result2 `elem` expectedResults
      status1 = if isPassing1 then "PASS ✓" else "FAIL ✗"
      status2 = if isPassing2 then "PASS ✓" else "FAIL ✗"

  putStrLn $ "\n" ++ testName ++ ":"
  putStrLn $ "  Input:    \"" ++ input ++ "\""
  putStrLn $ "  Expected: " ++ showExpected expectedResults
  putStrLn $ "  Solution 1 (Expand Center):"
  putStrLn $ "    Output: \"" ++ result1 ++ "\""
  putStrLn $ "    Status: " ++ status1
  putStrLn $ "  Solution 2 (Dynamic Programming):"
  putStrLn $ "    Output: \"" ++ result2 ++ "\""
  putStrLn $ "    Status: " ++ status2
  where
    showExpected [x] = "\"" ++ x ++ "\""
    showExpected xs = "One of: " ++ show xs

-- Soltuion 2: Dynamic Programming + Rolling Array
-- Time complexity: O(n^2)
-- Space complexity: O(n) for the array
type PalindromeLayer = Map.Map Int Bool

layer1 :: Int -> PalindromeLayer
layer1 n = Map.fromList [(i, True) | i <- [0 .. n - 1]]

layer2 :: Array Int Char -> Int -> PalindromeLayer
layer2 strArr n =
  Map.fromList
    [(i, strArr ! i == strArr ! (i + 1)) | i <- [0 .. n - 2]]

buildLayer ::
  Array Int Char -> -- The input string as an array
  Int -> -- Length of the string
  PalindromeLayer -> -- Previous layer (length k-2)
  Int -> -- Current length k
  PalindromeLayer -- New layer (length k)
buildLayer strArr n layerKMinus2 k =
  Map.fromList
    [ ( i,
        strArr ! i == strArr ! (i + k - 1)
          && Map.findWithDefault False (i + 1) layerKMinus2
      )
    | i <- [0 .. n - k]
    ]

-- Style guide: Use lowerCamelCase for functions
longestPalindromeConcise :: String -> String
longestPalindromeConcise s
  | n <= 1 = s
  | otherwise =
      let strArr = listArray (0, n - 1) s
          l1 = layer1 n
          l2 = layer2 strArr n

          initialBest =
            case find snd (Map.toList l2) of
              Just (start, _) -> (2, start)
              Nothing -> (1, 0)

          go k layerKMinus1 layerKMinus2 bestSoFar
            | k > n = bestSoFar
            | otherwise =
                let currLayer = buildLayer strArr n layerKMinus2 k

                    updatedBest = case find snd (Map.toList currLayer) of
                      Just (start, _) -> (k, start)
                      Nothing -> bestSoFar
                 in -- Pass parameters in the correct order
                    go (k + 1) currLayer layerKMinus1 updatedBest

          (finalStart, finalLen) = go 3 l2 l1 initialBest
       in take finalLen (drop finalStart s)
  where
    n = length s