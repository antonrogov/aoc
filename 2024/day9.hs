import Data.Char (digitToInt)

build input = fst $ foldl fn ([], True, 1) input
  where
    fst (res, _, _) = res
    fn (res, isFile, id) n =
      if isFile then
        (res ++ [[id, (digitToInt n)]], False, id + 1)
      else
        (res ++ [[0, (digitToInt n)]], True, id)

compact [] = []
compact n =
  let [id, len] = last n
  in if id == 0
     then compact (init n) ++ [[id, len]]
     else let (skip, found) = span (\ [i, l] -> i > 0 || l < len) (init n)
          in if (null found)
             then compact (init n) ++ [[id, len]]
             else let ([_, nlen] : rest) = found
                  in compact (skip ++ ([id, len] : [0, nlen - len] : rest)) ++ [[0, len]]

checksum _ [] = 0
checksum pos ([id, len]:ns)
  | id == 0 || len == 0 = checksum (pos + len) ns
  | otherwise = (id - 1) * pos + checksum (pos + 1) ([id, len - 1] : ns)


main = do
  text <- readFile "day9.txt"
  print $ checksum 0 $ compact $ build $ init text
