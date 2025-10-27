-- Leetcode 139. Word Break
-- @tag: need-review

module Solution where

import Data.Array
import Data.List (isPrefixOf)
import qualified Data.Set as Set

-- Solution without memorization
wordBreak :: String -> [String] -> Bool
wordBreak [] _ = True
wordBreak _ [] = False
wordBreak s dict = go s
  where
    go [] = True
    go x = or candidates
      where
        candidates =
          [ go (drop (length dictStr) x)
          | dictStr <- dict,
            dictStr `isPrefixOf` x
          ]

-- DP solution
wordBreakMemo :: String -> [String] -> Bool
wordBreakMemo s wordDict =
  let n = length s

      -- memo is an array where memo!i is True if s[i..n-1] can be broken.
      memo = array (0, n) [(i, solve i) | i <- [0 .. n]]

      -- solve finds if the suffix of `s` starting at index `i` can be broken.
      solve i
        | i == n = True -- Base case: we've successfully consumed the whole string.
        | otherwise =
            -- Check if any word in the dict is a prefix of the current suffix,
            -- and if the rest of the string after that word can also be broken.
            or
              [ memo ! (i + length word)
              | word <- wordDict,
                word `isPrefixOf` drop i s
              ]
   in memo ! 0 -- The answer is whether the whole string (suffix from index 0) can be broken.

-- Test helper function that runs both functions and compares results
runTest :: String -> [String] -> Bool -> IO ()
runTest s dict expected = do
  let result1 = wordBreak s dict
      result2 = wordBreakMemo s dict
      bothCorrect = result1 == expected && result2 == expected
      bothSame = result1 == result2
      status = if bothCorrect && bothSame then "PASS" else "FAIL"
  putStrLn $ status ++ ": input=(\"" ++ s ++ "\", " ++ show dict ++ ")"
  putStrLn $ "  Expected: " ++ show expected
  putStrLn $ "  wordBreak: " ++ show result1
  putStrLn $ "  wordBreakMemo: " ++ show result2
  putStrLn $ "  Both functions agree: " ++ show bothSame
  putStrLn ""

-- Main function with test cases
main :: IO ()
main = do
  putStrLn "Testing both wordBreak functions:"
  putStrLn "=================================\n"

  runTest "leetcode" ["leet", "code"] True
  runTest "applepenapple" ["apple", "pen"] True
  runTest "catsandog" ["cats", "dog", "sand", "and", "cat"] False
  runTest "" [] True
  runTest "a" [] False
  runTest "abcd" ["a", "abc", "b", "cd"] True
  runTest "cars" ["car", "ca", "rs"] True

  putStrLn "All tests completed!"
