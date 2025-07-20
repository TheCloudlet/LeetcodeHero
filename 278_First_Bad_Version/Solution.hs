{-
Leetcode 278 — First Bad Version

What I learned from this implementation:

1. Avoid using global functions like `isBadVersion :: Int -> Bool`
   in Haskell unless necessary. Even though it compiles,
   it makes the function `firstBadVersion` tightly coupled and less testable.

2. Prefer writing high-order functions with explicit dependencies:
   `firstBadVersion :: (Int -> Bool) -> Int -> Int`
   This makes the logic pure, testable, and reusable.nt -> IO Bool
-}

firstBadVersion :: (Int -> Bool) -> Int -> Int
firstBadVersion isBadVersion n = go 0 n -- keep `n` explicit for readability
  where
    go left right
      | left >= right = left
      | not (isBadVersion mid) = go (mid + 1) right
      | otherwise = go left mid
      where mid = left + (right - left) `div` 2

main :: IO()
main = do
  print $ firstBadVersion (>= 5) 10  -- output: 5
  print $ firstBadVersion even 10            -- output: 0
