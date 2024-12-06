import Text.Regex.PCRE
import Data.List.Split (splitOn)

readm :: [String] -> Int
readm [_, a, b] = read(a) * read(b)

process :: String -> Int
process s = sum $ map readm $ (s =~ "mul\\((\\d{1,3}),(\\d{1,3})\\)" :: [[String]])

processToggled (x:xs)
  | null xs = 0
  | otherwise = sum $ map process xs

main :: IO ()
main = do
  text <- readFile "day3.txt"
  print $ process text
  print $ sum $ map (processToggled . (splitOn "do()")) $ splitOn "don't()" ("do()" ++ text)
