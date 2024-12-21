import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashMap.Strict as Map
import Data.List.Split (splitOn)

dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]

path bs w h = calc (PQ.singleton 0 [0, 0]) Map.empty Map.empty
  where
    end = [w - 1, h - 1]
    calc pq cs parents
      | PQ.null pq = []
      | p == end = backtrack p
      | otherwise = calc pq' cs' parents'
      where
        less (p, c) = case Map.lookup p cs of
                        Just c' -> c < c'
                        Nothing -> True
        neighbors (p, c) =
          [(p', c + 1) | d <- dirs, let p' = zipWith (+) p d, isValidPos p']
        (c, p) = PQ.findMin pq
        ns = filter less $ neighbors (p, c)
        pq' = foldl (\q (p, c) -> PQ.insert c p q) (PQ.deleteMin pq) ns
        cs' = foldl (\m (p, c) -> Map.insert p c m) cs ((p, c) : ns)
        parents' = foldl (\m (n, c) -> Map.insert n p m) parents ns
        backtrack p =
          case Map.lookup p parents of
            Just p' -> p : backtrack p'
            Nothing -> []
    isValidPos [x, y] = x >= 0 && y >= 0 && x < w && y < h && [x, y] `notElem` bs

hasPath start end bs w h = calc (PQ.singleton 0 start) Map.empty
  where
    calc pq cs
      | PQ.null pq = False
      | p == end = True
      | otherwise = calc pq' cs'
      where
        less (p, c) = case Map.lookup p cs of
                        Just c' -> c < c'
                        Nothing -> True
        neighbors (p, c) =
          [(p', c + 1) | d <- dirs, let p' = zipWith (+) p d, isValidPos p']
        (c, p) = PQ.findMin pq
        ns = filter less $ neighbors (p, c)
        pq' = foldl (\q (p, c) -> PQ.insert c p q) (PQ.deleteMin pq) ns
        cs' = foldl (\m (p, c) -> Map.insert p c m) cs ((p, c) : ns)
    isValidPos [x, y] = x >= 0 && y >= 0 && x < w && y < h && [x, y] `notElem` bs

firstBlocking w h bs n =
  if hasPath [0, 0] [w - 1, h - 1] (take n bs) w h
  then firstBlocking w h bs (n + 1)
  else bs !! (n - 1)

main = do
   text <- readFile "day18.txt"
   let bs = map (map read . splitOn ",") $ lines text :: [[Int]]
   -- print $ length $ path (take 1024 bs) 71 71
   print $ firstBlocking 71 71 bs 1024
