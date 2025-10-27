-- | Leecode 12. Integer to Roman
intToRoman :: Int -> Maybe String
intToRoman n
  | n <= 0 || n > 3999 = error "Input out of Roman numeral range (1–3999)"
  | otherwise = Just $ go n romanPairs
  where
    go 0 _ = "" -- Base case: 完成轉換
    go _ [] = "" -- 代表 Roman Pair 不足，屬於錯誤情況
    go m ((val, sym) : rest)
      | m >= val = sym ++ go (m - val) ((val, sym) : rest)
      | otherwise = go m rest

romanPairs :: [(Int, String)]
romanPairs =
  [ (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I")
  ]

main :: IO ()
main =
  mapM_ printResult [3, 58, 1994, 3999, 0]
  where
    printResult n = case intToRoman n of
      Just s -> putStrLn $ show n ++ " -> " ++ s
      Nothing -> putStrLn $ show n ++ " is out of valid range!"
