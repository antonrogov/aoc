import Data.List (partition)

dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]

isValidPos field x y = x >= 0 && y >= 0 && x < length field && y < length (head field)

charAt field x y
  | isValidPos field x y = field !! y !! x
  | otherwise = ' '

neighbors field x y =
  let c = charAt field x y
  in [charAt field (x + dx) (y + dy) == c | [dx, dy] <- dirs]

sides field [x, y] =
  -- 4 - (length $ filter id $ neighbors field x y)
  let [n, e, s, w] = neighbors field x y
      wn = neighbors field (x - 1) y
      nn = neighbors field x (y - 1)
      ss = [not n && (not w || wn !! 0),
            not e && (not n || nn !! 1),
            not s && (not w || wn !! 2),
            not w && (not n || nn !! 3)]
  in length $ filter id ss

prepare field =
  [([x, y], c, sides field [x, y]) | y <- [0..length field - 1],
                                     x <- [0..length (field !! y) - 1],
                                     let c = charAt field x y]

isConnected ([x, y], c, _) ([x', y'], c', _) =
  c == c' && (x == x' && abs (y - y') == 1 || y == y' && abs (x - x') == 1)

lst (_, _, s) = s

findRegion rs ps =
  let (ns, ps') = partition (\p -> any (\r -> isConnected p r) (head rs)) ps
  in if null ns
       then (map lst $ concat rs, ps')
       else findRegion (ns : rs) ps'

process [] = []
process (c:cs) = let (rs, cs') = findRegion [[c]] cs
                 in rs : process cs'

price rs = sum rs * length rs

main = do
  text <- readFile "day12.txt"
  print $ sum $ map price $ process $ prepare $ linex text
