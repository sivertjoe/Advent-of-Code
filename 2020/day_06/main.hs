import Data.List
import Text.Printf
import System.CPUTime (getCPUTime)

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

myRound x = fromIntegral $ round  x

time :: ([String] -> Int) -> [String] -> IO Int
time f arg = do
    start <- getCPUTime
    let v = f arg
    end <- getCPUTime
    let diff = myRound $ (fromIntegral (end - start)) / (10^9)
    -- printf "(%0.3fms)\t"  (diff :: Double)
    printf "(%dms)\t" (diff :: Int)

    return v


main = do
  lines <- fmap lines (readFile "input")
  p1 <- time partOne lines
  print p1
  p2 <- time partTwo lines
  print p2
