import Data.List (sort, group)

isValidPos field x y = x >= 0 && y >= 0 && x < length (head field) && y < length field

charAt field x y
  | isValidPos field x y = field !! y !! x
  | otherwise = ' '

antidistsFor field x y = [[x - x', y - y'] | x' <- [0..length (head field) - 1],
                                             y' <- [0..length field - 1],
                                             x' /= x || y' /= y,
                                             charAt field x' y' == c]
  where c = charAt field x y

flatten = foldl (++) []

antinodesFor field x y = flatten $ map (takeWhile valid . advance) $ antidistsFor field x y
  where valid [x, y] = isValidPos field x y
        advance [dx, dy] = [[x + i * dx, y + i * dy] | i <- [0..]]

antinodes field = [antinodesFor field x y | y <- [0..length field - 1],
                                            x <- [0..length (head field) - 1],
                                            charAt field x y /= '.']

main = do
  text <- readFile "day8.txt"
  let field = lines text
  print $ length $ group $ sort $ flatten $ antinodes field
