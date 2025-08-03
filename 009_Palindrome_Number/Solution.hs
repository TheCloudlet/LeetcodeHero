-- | Leetcode 9. Palindrome Number
isPalindrome :: Int -> Bool
isPalindrome a = s == reverse s
  where
    s = show a

main :: IO ()
main = mapM_ printResult testCases
  where
    testCases = [-121, 121, 0, 12345654321]
    printResult n = putStrLn $ show n ++ " -> " ++ show (isPalindrome n)
