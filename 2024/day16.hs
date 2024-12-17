import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashMap.Strict as Map
import Data.List (sortBy, sort, group)
import Data.Ord (comparing)

dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]

charAt [x, y] field = field !! y !! x

find field c =
  head [[x, y] | y <- [0..length field - 1], x <- [0..length (field !! y) - 1],
                 field !! y !! x == c]

analyze start field = calc (PQ.singleton 0 start) Map.empty Map.empty
  where
    calc pq cs parents
      | PQ.null pq = (cs, parents)
      | otherwise = calc pq' cs' parents'
      where
        more (p, c) = case Map.lookup p cs of
                        Just c' -> c > c'
                        Nothing -> False
        less (p, c) = case Map.lookup p cs of
                        Just c' -> c < c'
                        Nothing -> True
        equal (p, c) = case Map.lookup p cs of
                        Just c' -> c == c'
                        Nothing -> False
        neighbors ((p, d), c) =
          let p' = zipWith (+) p (dirs !! d)
              ch = charAt p' field
              ps = if ch == '.' || ch == 'E'
                    then [((p', d), c + 1)]
                    else []
          in ps ++ [((p, d'), c + c') | d' <- [0..length dirs - 1],
                                        d' /= d,
                                        let c' = if abs (d - d') == 2
                                                  then 2000
                                                  else 1000]
        (c, p) = PQ.findMin pq
        ns = neighbors (p, c)
        pq' = foldl (\q (p, c) -> PQ.insert c p q) (PQ.deleteMin pq) $ filter less ns
        cs' = foldl (\m (p, c) -> Map.insert p c m) cs ((p, c) : filter less ns)
        parents' = foldl (\m (n, c) -> if less (n, c)
                                       then Map.insert n [p] m
                                       else if equal (n, c)
                                            then Map.update (\ps -> Just (p : ps)) n m
                                            else m) parents ns

backtrack parents p =
  case Map.lookup p parents of
    Just ps -> p : concatMap (backtrack parents) ps
    Nothing -> [p]

main = do
   text <- readFile "day16.txt"
   let field = lines text
       start = (find field 'S', 1)
       end = find field 'E'
       (costs, parents) = analyze start field
       ends = [(d, Map.lookup (end, d) costs) | d <- [0..length dirs - 1]]
       (dir, cost) = head $ sortBy (comparing snd) ends
   print cost
   print $ length $ group $ sort $ map fst $ backtrack parents (end, dir)
