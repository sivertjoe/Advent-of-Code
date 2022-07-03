import Data.List

--         xs        res  list  init   sum           update
count :: [String] -> Int -> i -> i -> (i -> Int) -> (i -> String -> i) -> Int
count [] res list _ sum _ =
  res + sum list
count (x : xs) res list init sum update = case x of
  "" -> count xs (res + sum list) init init sum update
  str -> count xs res (update list str) init sum update

partOne :: [String] -> Int
partOne lines =
  count lines 0 [] [] (length . nub) (++)

partTwo :: [String] -> Int
partTwo lines =
  count
    lines
    0
    Nothing
    Nothing
    (maybe 0 (length . nub))
    (\list str -> maybe (Just str) (Just . (intersect str)) list)

main = do
  lines <- fmap lines (readFile "input")
  print $ partOne lines
  print $ partTwo lines