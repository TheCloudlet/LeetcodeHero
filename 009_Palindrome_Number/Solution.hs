-- | Leetcode 9. Palindrome Number

isPalindrome :: Int -> Bool
isPalindrome a = s == reverse s where
  s = show a

main :: IO ()
main = do
  print $ isPalindrome (-121)      -- False
  print $ isPalindrome 121         -- True
  print $ isPalindrome 0           -- True
  print $ isPalindrome 12345654321 -- True
