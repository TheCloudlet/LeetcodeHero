{-|
Leetcode 35. Search Insert Position
Binary Search for Lower Bound

== Goal
Find the index where @target@ should be inserted to maintain the sorted order.

* If @target@ is found in the array, return its index
* Otherwise, return the index where it can be inserted

This implementation uses binary search on a sorted list.
Note: accessing list elements with @!!@ has O(n) time complexity per access,
so the total complexity is O(n log n). Use @Vector@ if you need real O(log n).
-}

searchInsert :: [Int] -> Int -> Int
searchInsert nums target = go 0 (length nums) where
  go :: Int -> Int -> Int
  go low high
    | low >= high = low
    | nums !! mid >= target = go low mid
    | otherwise = go (mid + 1) high
      where mid = low + (high - low) `div` 2


main :: IO()
main = do
  print $ searchInsert [1, 2, 3, 5] 0
  print $ searchInsert [1, 2, 3, 5] 1
  print $ searchInsert [1, 2, 3, 5] 2
  print $ searchInsert [1, 2, 3, 5] 3
  print $ searchInsert [1, 2, 3, 5] 4
  print $ searchInsert [1, 2, 3, 5] 5
  print $ searchInsert [1, 2, 3, 5] 6

{-|
  Binary Search in Haskell — Notes on List vs Vector

  When implementing binary search on a list like [Int],
  avoid using the indexing operator (!!) inside recursion:

    nums !! mid  -- O(n) access time!

  Since Haskell's lists are linked lists, indexing is linear time.
  This causes a binary search to degrade from O(log n) to O(n log n),
  because every recursive step must scan the list to reach mid.

  ✅ Instead, prefer using `Data.Vector` or `Data.Vector.Unboxed`,
     which supports O(1) random access, just like C++'s std::vector:

     - Use `V.fromList` to convert a list to a vector
     - Use `v V.! i` to index

  Example:
    import qualified Data.Vector as V

    searchInsert :: V.Vector Int -> Int -> Int
    searchInsert v target = go 0 (V.length v)
      where
        go low high
          | low >= high = low
          | v V.! mid >= target = go low mid
          | otherwise = go (mid + 1) high
          where mid = low + (high - low) `div` 2

  TL;DR:
    - Avoid (!!) in performance-critical binary search
    - Use Vector if you need O(log n) total time
    - Lists are fine for clarity or small inputs
-}
