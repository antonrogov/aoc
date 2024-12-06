import Data.List (sort, group)

charAt field x y
  | x < 0 || y < 0 || x > length field - 1 || y > length (head field) - 1 = ' '
  | otherwise = field !! y !! x

charToDir '^' = [0, -1]
charToDir 'v' = [0, 1]
charToDir '<' = [-1, 0]
charToDir '>' = [1, 0]

rotate [0, -1] = [1, 0]
rotate [1, 0] = [0, 1]
rotate [0, 1] = [-1, 0]
rotate [-1, 0] = [0, -1]

findPos field =
  head [(x, y) | (y, line) <- zip [0..] field,
                 (x, char) <- zip [0..] line,
                 char == '^' || char == 'v' || char == '<' || char == '>']

getState field =
  let (x, y) = findPos field
      char = charAt field x y
      [dx, dy] = charToDir char
  in [x, y, dx, dy]

step field states =
  let [x, y, dx, dy] = head states
      nx = x + dx
      ny = y + dy
  in
    case charAt field nx ny of
      ' ' -> states
      '#' -> step field (([x, y] ++ rotate [dx, dy]) : (tail states))
      _ -> step field ([nx, ny, dx, dy] : states)

hasLoop field ox oy states =
  let [x, y, dx, dy] = head states
      nx = x + dx
      ny = y + dy
      next = charAt field nx ny
  in
    if elem [x, y, dx, dy] (tail states) then
      True
    else if next == ' ' then
      False
    else if next == '#' || nx == ox && ny == oy then
      hasLoop field ox oy (([x, y] ++ rotate [dx, dy]) : (tail states))
    else
      hasLoop field ox oy ([nx, ny, dx, dy] : states)

potentialBlocks field =
  [[x, y] | (y, line) <- zip [0..] field,
            (x, char) <- zip [0..] line,
            char == '.']

main = do
  text <- readFile "day6.txt"
  let field = lines text
      state = getState field
  print $ length $ group $ sort $ map (take 2) $ step field [state]
  -- TODO: optimize as this took 59 minutes
  print $ length $ filter (\[x, y] -> hasLoop field x y [state]) $ potentialBlocks field
