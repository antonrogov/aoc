import Text.Regex.PCRE
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Ratio

check :: [[Ratio Int]] -> Maybe Int
check [[ax, ay], [bx, by], [px', py']] =
  let px = px' + 10000000000000
      py = py' + 10000000000000
      a = (py - px * by / bx) / (ay - ax * by / bx)
      b = (px - a * ax) / bx
  in if denominator a == 1 && denominator b == 1
       then Just (numerator a * 3 + numerator b)
       else Nothing

parseLine :: String -> [Ratio Int]
parseLine s =
  let [res] = s =~ "(\\d+).*?(\\d+)" :: [[String]]
  in map (\n -> read n % 1) $ tail res

parseBlock b =
  map parseLine b

main = do
   text <- readFile "day13.txt"
   print $ sum $ map (fromMaybe 0 . check . parseBlock) $ splitOn [""] $ lines text
