
import Data.List
import System.IO

listDistance :: [Int] -> [Int] -> Int
listDistance xs ys = foldr (\(x,y) r -> r + abs (x - y)) 0
  $ zip (sort xs) (sort ys)

split :: [a] -> ([a],[a])
split xs = splitHelper xs ([], [])
  where splitHelper []       (l1, l2) = (l1, l2)
        splitHelper (x : xs) (l1, l2) = splitHelper xs (l2, x : l1)

similarityScore :: [Int] -> [Int] -> Int
similarityScore xs ys = sum [ x * occurence x ys | x <- xs ]
  where occurence w []       = 0
        occurence w (v : vs) | w == v    = 1 + occurence w vs
                             | otherwise = occurence w vs

main :: IO()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let numbers = (fmap read $ words contents) :: [Int]
      (l1, l2) = split numbers
  putStrLn . show $ listDistance l1 l2
  putStrLn . show $ similarityScore l1 l2
