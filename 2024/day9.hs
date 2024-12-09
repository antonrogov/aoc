import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

build input = fst $ foldl fn ([], True, 0) input
  where
    fst (res, _, _) = res
    fn (res, isFile, id) n =
      if isFile then
        (res ++ replicate (digitToInt n) (Just id), False, id + 1)
      else
        (res ++ replicate (digitToInt n) Nothing, True, id)

compact [] = []
compact [n] = [n]
compact (Just n:ns) = Just n : compact ns
compact (Nothing:ns) =
  case last ns of
      Just n -> Just n : compact rest
      Nothing -> compact (Nothing : rest)
  where rest = init ns

checksum cs = sum $ zipWith (*) [0..] $ map (fromMaybe 0) cs

main = do
  text <- readFile "day9.txt"
  print $ checksum $ compact $ build $ init text
