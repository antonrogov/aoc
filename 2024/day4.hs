dirs = [[1, 0], [-1, 0], [0, 1], [0, -1], [1, 1], [-1, -1], [1, -1], [-1, 1]]

charAt field x y
  | x < 0 || y < 0 || x > length (head field) - 1 || y > length field - 1 = ' '
  | otherwise = field !! y !! x

word field x y [dx, dy] = [charAt field (x + i * dx) (y + i * dy) | i <- [0..3]]

countXmasAt field x y = length $ filter (== "XMAS") $ map (\d -> word field x y d) dirs

countMasAt field x y
  | charAt field x y == 'A' &&
    ((charAt field (x + 1) (y + 1) == 'M' && charAt field (x - 1) (y - 1) == 'S' ||
      charAt field (x + 1) (y + 1) == 'S' && charAt field (x - 1) (y - 1) == 'M') &&
     (charAt field (x - 1) (y + 1) == 'M' && charAt field (x + 1) (y - 1) == 'S' ||
      charAt field (x - 1) (y + 1) == 'S' && charAt field (x + 1) (y - 1) == 'M')) = 1
  | otherwise = 0

countWith count field =
  sum [count field x y | x <- [0..length (head field) - 1], y <- [0..length field - 1]]

main = do
  text <- readFile "day4.txt"
  let field = lines text
  print $ countWith countXmasAt field
  print $ countWith countMasAt field
