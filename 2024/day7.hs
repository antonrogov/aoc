import Data.List.Split (splitOn)

combine :: Int -> Int -> Int
combine b a = read (show a ++ show b)

results [n] = [n]
results (n:ns) = let ns' = results ns
                 in (map (+ n) ns') ++ (map (* n) ns') ++ (map (combine n) ns')

isValid :: Int -> [Int] -> Bool
isValid v ns = elem v $ results $ reverse ns

process line =
  let [vs, nss] = splitOn ": " line
      v = read vs
  in
    if isValid v (map read $ splitOn " " nss)
    then v
    else 0

main = do
  text <- readFile "day7.txt"
  print $ sum $ map process $ lines text
