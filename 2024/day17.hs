import Data.Bits (xor)
import Data.List (find)

process :: [Int] -> [Int] -> Int -> [Int]
process [a, b, c] is p
  | p >= length is = []
  | otherwise =
    case is !! p of
        0 -> process [a `div` (2 ^ combo), b, c] is (p + 2)
        1 -> process [a, b `xor` v, c] is (p + 2)
        2 -> process [a, combo `mod` 8, c] is (p + 2)
        3 -> if a == 0
             then process [a, b, c] is (p + 2)
             else process [a, b, c] is v
        4 -> process [a, b `xor` c, c] is (p + 2)
        5 -> combo `mod` 8 : process [a, b, c] is (p + 2)
        6 -> process [a, a `div` (2 ^ combo), c] is (p + 2)
        7 -> process [a, b, a `div` (2 ^ combo)] is (p + 2)
  where
    v = is !! (p + 1)
    combo
      | v <= 3 = v
      | v == 4 = a
      | v == 5 = b
      | v == 6 = c
      | otherwise = error ("Invalid combo " ++ show v)

findQuine is = find' [] (reverse is) 0
  where
    find' _ [] a = [a]
    find' is' (v:vs) a = concatMap (\a' -> find' is'' vs (a * 8 + a')) as'
      where is'' = v : is'
            as' = filter (\a' -> process [a * 8 + a', 0, 0] is 0 == is'') [0..7]

main = do
  let is = [2,4,1,1,7,5,4,4,1,4,0,3,5,5,3,0]
  print $ process [46337277, 0, 0] is 0
  print $ minimum $ findQuine is
