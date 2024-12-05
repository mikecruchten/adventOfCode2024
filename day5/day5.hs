import System.IO
import Data.List.Split

type Rel = [(Int,Int)]

type Update = [Int]

toRelation :: [String] -> Rel
toRelation = fmap pair
  where numbers s = splitOn "|" s
        pair s = ( read $ numbers s !! 0 :: Int
                 , read $ numbers s !! 1 :: Int)

toUpdate :: String -> Update
toUpdate us = foldr (\n r -> (read n :: Int) : r) [] $ splitOn "," us

orderedAccTo :: Update -> Rel -> Bool
orderedAccTo []             _ = True
orderedAccTo [x]            _ = True
orderedAccTo (x1 : x2 : xs) r | (x1,x2) `elem` r = orderedAccTo (x2 : xs) r
                              | otherwise        = False

getMiddleEntry :: Update -> Int
getMiddleEntry u = u !! (length u `div` 2)

mergeSort :: Rel -> Update -> Update
mergeSort _ []       = []
mergeSort r (x : xs) = (mergeSort r [ y | y <- xs, (y,x) `elem` r ])
                    ++ (x : (mergeSort r [ y | y <- xs, (x,y) `elem` r ]))

score :: [Update] -> Int
score = sum . (fmap getMiddleEntry)

main :: IO()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
      rules = toRelation $ takeWhile (not . null) ls
      updates = fmap toUpdate $ drop 1 (dropWhile (not . null) ls)
      orderedUpdates = filter (`orderedAccTo` rules) updates
      unorderedUpdates = filter (not . (`orderedAccTo` rules)) updates
  putStrLn . show $ score orderedUpdates
  putStrLn . show
    $ score
    $ (fmap (mergeSort rules))
    $ unorderedUpdates
