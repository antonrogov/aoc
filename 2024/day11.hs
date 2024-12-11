stone s
  | s == 0 = [1]
  | even (length (show s)) = let ss = show s
                                 l2 = (length ss) `div` 2
                             in [read $ take l2 ss, read $ drop l2 ss]
  | otherwise = [s * 2024]

process ss = concat $ map stone ss

main = do
   text <- readFile "day11.txt"
   let ss = (map read $ words text) :: [Integer]
   print $ length $ iterate process ss !! 25
