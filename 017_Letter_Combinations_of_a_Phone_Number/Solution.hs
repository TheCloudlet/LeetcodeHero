-- Leetcode 17. Letter Combinations of a Phone Number

module Solution where

import qualified Data.Map as Map

digitToLettersMap :: Map.Map Char String
digitToLettersMap =
  Map.fromList
    [ ('2', "abc"),
      ('3', "def"),
      ('4', "ghi"),
      ('5', "jkl"),
      ('6', "mno"),
      ('7', "pqrs"),
      ('8', "tuv"),
      ('9', "wxyz")
    ]

letterCombinations :: String -> [String]
letterCombinations "" = []
letterCombinations (c : cs) =
  -- Safely look up the letters for the current digit
  case Map.lookup c digitToLettersMap of
    -- If the digit isn't in the map (e.g., '1'), skip it and process the rest
    Nothing -> letterCombinations cs
    -- If we found letters (e.g., Just "abc")
    Just letters ->
      -- If this is the last digit, its letters are the combinations
      if null cs
        then [[l] | l <- letters]
        -- Otherwise, combine the letters with the combinations from the rest of the digits
        else [l : rest | l <- letters, rest <- letterCombinations cs]

printHelper :: String -> IO ()
printHelper numStr = do
  let result = letterCombinations numStr
  putStrLn $ "Input: \"" ++ numStr ++ "\""
  putStr "Output: "
  print result
  putStrLn "" -- Add a blank line for better separation

-- Main function to run the test cases.
main :: IO ()
main = do
  putStrLn "--- Testing Letter Combinations of a Phone Number ---"

  -- Test Case 1: Standard case with two digits
  printHelper "23"

  -- Test Case 2: Empty input string
  printHelper ""

  -- Test Case 3: Single digit input
  printHelper "2"

  putStrLn "--- End of Tests ---"
