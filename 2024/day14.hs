import Text.Regex.PCRE

pos n w h [x, y, vx, vy] = [(x + n * vx) `mod` w, (y + n * vy) `mod` h]

safety w h ps =
  product [f (< w2) (< h2), f (> w2) (< h2), f (< w2) (> h2), f (> w2) (> h2)]
  where w2 = w `div` 2
        h2 = h `div` 2
        f fx fy = length $ filter (\ [x, y] -> fx x && fy y) ps

process n w h rs = safety w h $ map (pos n w h) rs

place w h ps = map (\y -> map (\x -> [x, y] `elem` ps) [0..w-1]) [0..h-1]

display n ls = show n ++ "\n" ++ (unlines $ map (map (\c -> if c then '#' else '.')) ls)

hasSpan s line
  | length line < s = False
  | all id $ take s line = True
  | otherwise = hasSpan s (tail line)

findTree n s w h rs
  | n > w * h = "NONE"
  | otherwise = let ls = place w h $ map (pos n w h) rs
                in if any (hasSpan s) ls
                   then display n ls
                   else findTree (n + 1) s w h rs

parse :: String -> [Int]
parse line =
  let [res] = line =~ "p=([0-9-]+),([0-9-]+) v=([0-9-]+),([0-9-]+)" :: [[String]]
  in map read $ tail res

main = do
   text <- readFile "day14.txt"
   let rs = map parse $ lines text
   -- print $ process 100 101 103 rs
   putStr $ findTree 0 10 101 103 rs
