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
