import Data.List

tuple [x, y] = (x, y)

dist x y = abs $ x - y

day1 = do
   text <- readFile "day1.txt"
   let (xs, ys) = unzip . map (tuple . map read . words) . lines $ text
   print $ sum $ zipWith dist (sort xs) (sort ys)
   print $ sum $ map (\x -> x * (length $ filter (== x) ys)) xs

main = day1
