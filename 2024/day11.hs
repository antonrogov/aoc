stone s
  | s == 0 = [1]
  | even (length (show s)) = let ss = show s
                                 l2 = (length ss) `div` 2
                             in [read $ take l2 ss, read $ drop l2 ss]
  | otherwise = [s * 2024]

inc n acc s = case lookup s acc of
  Just n' -> (s, n' + n) : filter (\ (s', _) -> s' /= s) acc
  Nothing -> (s, n) : acc

process cs =
  foldl (\acc (s, c) -> foldl (inc c) acc $ stone s) [] cs

main = do
   text <- readFile "day11.txt"
   let ss = (map read $ words text) :: [Integer]
   let cs = foldl (inc 1) [] ss
   print $ sum $ map snd $ iterate process cs !! 75
