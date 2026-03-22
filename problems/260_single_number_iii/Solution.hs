-- Leetcode 260. Single Number III
module Solution where

import Data.Bits (xor, (.&.))
import Data.List (foldl')

singleNumberFold :: [Int] -> [Int]
singleNumberFold nums = [a, b]
  where
    -- XOR all numbers to get a ^ b
    xorSum = foldl' xor 0 nums

    -- Isolate the lowest set bit of xorSum.
    -- In two's complement, -x = ~x + 1, so x .&. (-x) isolates the lowest set bit.
    mask = xorSum .&. (-xorSum)

    -- Fold over the list, accumulating (groupA, groupB) XOR results.
    -- Initial accumulator is (0, 0).
    (a, b) = foldl' step (0, 0) nums

    -- Helper: update the appropriate group based on the mask.
    step (accA, accB) x
      -- If x has the distinguishing bit set, XOR it into the first group.
      | (x .&. mask) /= 0 = (accA `xor` x, accB)
      -- Otherwise, XOR it into the second group.
      | otherwise         = (accA, accB `xor` x)
