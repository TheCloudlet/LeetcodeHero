-- Leetcode 268. Missing Number

module Solution where

import Data.Bits (xor)
import Data.List (foldl')

missingNumber :: [Int] -> Int
missingNumber nums = foldl' xor xor_sum nums
  where n = length nums
        xor_sum = foldl' xor 0 [0..n]

missingNumber2 :: [Int] -> Int
missingNumber2 nums = foldl' xor 0 ([0..n] ++ nums)
  where n = length nums
