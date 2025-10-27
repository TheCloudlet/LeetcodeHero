-- Leetcode 14. Longest Common Prefix
-- @tag: need-review

module Solution where

-- Example: transpose ["abc", "def"] = ["ad", "be", "cf"]
import Data.List (transpose)

-- | Solution using transpose (idiomatic Haskell approach)
longestCommonPrefix' :: [String] -> String
longestCommonPrefix' [] = ""
longestCommonPrefix' [""] = ""
longestCommonPrefix' [x] = x

-- 1. transpose strs - converts list of strings into list of characters at each --    position
--    Example: ["flower","flow"] -> [['f','f'],['l','l'],['o','o'],['w','w'],
--    ['e'],['r']]
-- 2. takeWhile same - takes characters as long as they're all the same at each
--    position
-- 3. The result is our longest common prefix
longestCommonPrefix strs = takeWhile same $ transpose strs
  where
    -- Helper function to check if all characters at a position are the same
    -- Uses 'all' to check if every character equals the first character
    same chars = all (== head chars) chars

printTestCase :: Int -> [String] -> String -> String -> IO ()
printTestCase n input expect output = do
  putStrLn $ "Testcase " ++ show n ++ ":"
  putStrLn $ "  Input:   " ++ show input
  putStrLn $ "  Expect:  " ++ show expect
  putStrLn $ "  Output:  " ++ show output
  putStrLn $ "  Pass?    " ++ show (output == expect)
  putStrLn ""

main :: IO ()
main = do
  let testcases =
        [ (["flower", "flow", "flight"], "fl"),
          (["dog", "racecar", "car"], ""),
          (["interspecies", "interstellar", "interstate"], "inters"),
          (["throne", "throne"], "throne"),
          ([""], ""),
          ([], ""),
          (["a"], "a"),
          (["ab", "a"], "a"),
          (["abc", "abc", "abc"], "abc"),
          (["prefix", "pre", "presume"], "pre")
        ]
  mapM_ (\(i, (input, expect)) -> printTestCase i input expect (longestCommonPrefix input)) $ zip [1 ..] testcases
