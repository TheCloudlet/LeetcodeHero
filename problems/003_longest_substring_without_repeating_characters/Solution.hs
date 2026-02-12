-- Leetcode 3. Longest Substring Without Repeating Characters
--
-- Algorithm: Sliding window (linear iterative process, SICP §1.2.1)
-- Time: O(n), Space: O(charset)
--
-- SICP Perspective:
--   - LINEAR ITERATIVE PROCESS: State evolves through each character
--   - State: (maxLen, leftBound, char→lastSeenIndex)
--   - Invariant: All chars in [left, i] are unique

module Solution where

import qualified Data.Map.Strict as M
import Data.List (fold')

-- | Returns length of longest substring without repeating characters.
-- Maintains sliding window [left, i] with uniqueness invariant.
lengthOfLongestSubstring :: String -> Int
lengthOfLongestSubstring s = best
  where
    -- State: (maxLen, leftBound, char→lastIndex)
    (best, _, _) = foldl' step (0, 0, M.empty) (zip [0..] s)

    step :: (Int, Int, M.Map Char Int) -> (Int, Char) -> (Int, Int, M.Map Char Int)
    step (best, left, mp) (i, c) =
      let -- Move left past duplicate (max needed for stale entries in map)
          left' = case M.lookup c mp of
            Just j -> max left (j + 1)
            Nothing -> left
          mp' = M.insert c i mp
          best' = max best (i - left' + 1)
       in (best', left', mp')

{- Test cases:
lengthOfLongestSubstring ""              -- 0
lengthOfLongestSubstring "a"             -- 1
lengthOfLongestSubstring "bbbbb"         -- 1
lengthOfLongestSubstring "abcabcbb"      -- 3   -- "abc"
lengthOfLongestSubstring "pwwkew"        -- 3   -- "wke"
lengthOfLongestSubstring "dvdf"          -- 3   -- "vdf"
lengthOfLongestSubstring "abba"          -- 2   -- "ab" or "ba" (why max needed)
-}
