isIncreasing :: Int -> Bool
isIncreasing l = l >= 1 && l <= 3

isDecreasing :: Int -> Bool
isDecreasing l = l >= -3 && l <= -1

isValid ls =
  let diffs = zipWith (-) ls (tail ls)
      in all isIncreasing diffs || all isDecreasing diffs

removeAt i xs = take i xs ++ drop (i + 1) xs

isValidExt ls =
  isValid ls || any isValid [removeAt i ls | i <- [0..length ls - 1]]

main = do
   text <- readFile "day2.txt"
   let rs = map (map read . words) . lines $ text
   print $ length $ filter isValid rs
   print $ length $ filter isValidExt rs