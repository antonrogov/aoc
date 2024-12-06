import Data.List (elemIndex, partition)
import Data.List.Split (splitOn)

isRuleCorrect update [a, b] =
  case (elemIndex a update, elemIndex b update) of
      (Just ai, Just bi) -> ai < bi
      _ -> True

middleItem xs = xs !! (length xs `div` 2)

fixRule update [a, b] =
  case (elemIndex a update, elemIndex b update) of
      (Just ai, Just bi) ->
        if ai > bi
        then
          let (h1, _:t1) = splitAt ai update
              (h2, _:t2) = splitAt bi h1
          in h2 ++ [a, b] ++ t2 ++ t1
        else update
      _ -> update

fixRules rules update =
  let fixed = foldl fixRule update rules
  in if all (isRuleCorrect fixed) rules
     then fixed
     else fixRules rules fixed

main = do
  text <- readFile "day5.txt"
  let (rs, _:us) = span (not . null) $ lines text
      rules = map (splitOn "|") rs
      updates = map (splitOn ",") us
      (correct, incorrect) = partition (\u -> all (isRuleCorrect u) rules) updates
      corrected = map (fixRules rules) incorrect

  print $ sum $ map read $ map middleItem correct
  print $ sum $ map read $ map middleItem corrected
