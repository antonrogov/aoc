import Data.Char (digitToInt, intToDigit)
import Data.List (group, sort)

dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]

charAt field x y
  | x < 0 || y < 0 || x > length field - 1 || y > length (head field) - 1 = ' '
  | otherwise = field !! y !! x

nexts field x y =
  let d = digitToInt $ charAt field x y
  in [[nx, ny] | [dx, dy] <- dirs,
                 let nx = x + dx
                     ny = y + dy,
                 charAt field nx ny == intToDigit (d + 1)]

uniq rs = map head $ group $ sort rs

routes field [x, y]
  | charAt field x y == '9' = [[[x, y]]]
  | otherwise = uniq $ concat $ map collect $ nexts field x y
                -- where collect [nx, ny] = routes field [nx, ny]
                where collect [nx, ny] = map (\r -> [x, y] : r) $ routes field [nx, ny]

starts field =
  [[x, y] | y <- [0..length field - 1], x <- [0..length (head field) - 1],
            charAt field x y == '0']

trailheads field =
  map (\p -> length $ routes field p) $ starts field

main = do
  text <- readFile "day10.txt"
  let field = lines text
  print $ sum $ trailheads field
