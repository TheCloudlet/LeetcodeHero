-- Leetcode 647. Palindromic Substrings

module Solution where

import Data.Array
import Data.List
import qualified Data.Map as Map

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

countSubstrings :: String -> Int
countSubstrings s
  | n <= 1 = n
  | otherwise = go 3 l1 l2 initCount
  where
    n = length s
    strArr = listArray (0, n - 1) s
    l1 = layer1 n
    l2 = layer2 strArr n

    countHelper = length . filter id . Map.elems

    initCount = countHelper l1 + countHelper l2

    go k layerKMinus2 layerKMinus1 palindromeCount
      | k > n = palindromeCount
      | otherwise =
          go (k + 1) layerKMinus1 currLayer (palindromeCount + currLayerCount)
      where
        currLayer = buildLayer strArr n layerKMinus2 k
        currLayerCount = countHelper currLayer

-- Test helper functions
testCase :: String -> Int -> IO ()
testCase input expected = do
  let result = countSubstrings input
  let status = if result == expected then "✓ PASS" else "✗ FAIL"
  putStrLn $ "Input: \"" ++ input ++ "\" | Expected: " ++ show expected ++ " | Got: " ++ show result ++ " | " ++ status

main :: IO ()
main = do
  putStrLn "-----------------------------------------------------"
  putStrLn "Testing Palindromic Substrings Counter"
  putStrLn "-----------------------------------------------------"

  -- Basic test cases
  testCase "" 0                    -- Empty string
  testCase "a" 1                   -- Single character
  testCase "aa" 3                  -- Two same characters: "a", "a", "aa"
  testCase "ab" 2                  -- Two different characters: "a", "b"

  -- LeetCode example cases
  testCase "abc" 3                 -- "a", "b", "c"
  testCase "aaa" 6                 -- "a", "a", "a", "aa", "aa", "aaa"

  -- More complex cases
  testCase "aba" 4                 -- "a", "b", "a", "aba"
  testCase "abccba" 9              -- Multiple palindromes
  testCase "racecar" 10            -- Palindrome with nested palindromes
  testCase "noon" 6                -- "n", "o", "o", "n", "oo", "noon"
  testCase "abcd" 4                -- No palindromes except single chars
  testCase "level" 7               -- Another palindrome
