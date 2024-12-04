import System.IO
import Data.List.Split

isSafe :: [Int] -> Bool
isSafe xs = and (zipWith helper xs (drop 1 xs))
  || and (zipWith (swap helper) xs (drop 1 xs))
  where helper x1 x2 = x1 < x2 && x2 - x1 <= 3
        swap f x1 x2 = f x2 x1

notExcessViolates :: [Int] -> (Int -> Int -> Bool) -> Bool
notExcessViolates (y1 : y2 : ys) p | p y1 y2   = notExcessViolates (y2 : ys) p
                                   | otherwise = and
                                       $ zipWith p (y1 : ys) (drop 1 (y1 : ys))
notExcessViolates _              p = True

isDampenerSafe :: [Int] -> Bool
isDampenerSafe xs = isSafe (tail xs)
  || xs `notExcessViolates` safetyCondition
  || xs `notExcessViolates` (swap safetyCondition)
  where safetyCondition x1 x2 = x1 < x2 && x2 - x1 <= 3
        swap f x1 x2 = f x2 x1

main :: IO()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let reports = (fmap ((fmap read) . (splitOn " ")) $ lines contents) :: [[Int]]
      safeReports = filter isSafe reports
      dampenerSafeReports = filter isDampenerSafe reports
  putStrLn . show $ length safeReports
  putStrLn . show $ length dampenerSafeReports
