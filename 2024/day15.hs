import Data.List.Split (splitOn)

isFree [x, y] field = field !! y !! x == '.'
isBox [x, y] field = field !! y !! x == 'O' || isBoxLeft [x, y] field || isBoxRight [x, y] field
isBoxLeft [x, y] field = field !! y !! x == '['
isBoxRight [x, y] field = field !! y !! x == ']'

charToDir '<' = [-1, 0]
charToDir '>' = [1, 0]
charToDir '^' = [0, -1]
charToDir 'v' = [0, 1]

step [x, y] [dx, dy] = [x + dx, y + dy]

replace [x, y] c field =
  let value = field !! y !! x
      row = field !! y
      newRow = take x row ++ [c] ++ drop (x + 1) row
  in take y field ++ [newRow] ++ drop (y + 1) field

move [x, y] [nx, ny] field =
  let c = field !! y !! x
  in replace [nx, ny] c $ replace [x, y] '.' field

tryPush pos dir field =
  let next = step pos dir
  in if isBox next field
     then snd $ tryMove next dir field
     else field

isVertical [_, dy] = dy /= 0

tryPush' pos dir field =
  let next = step pos dir
  in if isVertical dir
     then if isBoxLeft next field
          then case tryMoveN [next, (step next [1, 0])] dir field of
               Just field' -> field'
               Nothing -> field
          else if isBoxRight next field
               then case tryMoveN [(step next [-1, 0]), next] dir field of
                     Just field' -> field'
                     Nothing -> field
               else tryPush pos dir field
     else tryPush pos dir field

tryMove pos dir field =
  let cleaned = tryPush' pos dir field
      next = step pos dir
  in if isFree next cleaned
     then (next, move pos next cleaned)
     else (pos, field)

tryMoveN [] _ field = Just field
tryMoveN (pos:rest) dir field =
  let (pos', field') = tryMove pos dir field
  in if pos' /= pos
     then tryMoveN rest dir field'
     else Nothing


findRobot field =
  head [[x, y] | y <- [0..length field - 1],
                 x <- [0..length (head field) - 1],
                 field !! y !! x == '@']

run [] (pos, field) = field
run (m:ms) (pos, field) = run ms $ tryMove pos m field

scaleCell '#' = "##"
scaleCell 'O' = "[]"
scaleCell '.' = ".."
scaleCell '@' = "@."

scale field = map (concat . map scaleCell) field

sumGPS field =
  sum [y * 100 + x | y <- [0..length field - 1],
                     x <- [0..length (head field) - 1],
                     field !! y !! x == 'O' || field !! y !! x == '[']

main = do
   text <- readFile "day15.txt"
   let [field', ms] = splitOn [""] $ lines text
   let field = scale field'
   let pos = findRobot field
   let moves = map charToDir $ concat ms
   print $ sumGPS $ run moves (pos, field)
