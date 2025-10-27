-- Leetcode 17. Letter Combinations of a Phone Number

module Solution where

import qualified Data.Map as Map
import Data.Maybe (maybeToList)

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

-- A better way to write, using maybe
letterCombinations' :: String -> [String]
letterCombinations' "" = [""] -- 基本情況回傳包含空字串的列表
letterCombinations' (c : cs) = do
  -- 1. Look up the letters for the current digit.
  --    `maybeToList` converts `Nothing` to `[]` and `Just "abc"` to `["abc"]`.
  --    `letters` will be bound to "abc".
  letters <- maybeToList (Map.lookup c digitToLettersMap)

  -- 2. Iterate over each CHARACTER of the `letters` string.
  --    `letter` will be 'a', then 'b', then 'c'.
  letter <- letters

  -- 3. Recursively call to get the combinations for the rest of the digits.
  --    `rest` will be bound to each string in the result of the recursive call.
  rest <- letterCombinations' cs

  -- 4. Prepend the current character to the rest of the combination.
  return (letter : rest)

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
