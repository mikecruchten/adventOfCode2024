import System.IO
import Data.List.Split
import Data.List

intConcat :: Integer -> Integer -> Integer
intConcat n m = read $ (show n) ++ (show m) :: Integer

isValid :: (Integer, [Integer]) -> Bool
isValid (x, [])       = x == 0
isValid (x, (y : ys)) | x <= 0     = False
                      | m /= 0    = isValid (x - y, ys)
                      | otherwise = isValid (d, ys) || isValid (x - y, ys)
  where (d,m) = divMod x y

isValid' :: (Integer, [Integer]) -> Bool
isValid' (x, [])             = x == 0
isValid' (x, [y])            = x == y
isValid' (x, (y1 : y2 : ys)) = isValid' (x, (y1 * y2 : ys))
                                || isValid' (x, (y1 + y2 : ys))
                                || isValid' (x, (intConcat y1 y2 : ys))

main :: IO()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
      d = fmap
        (\xs ->
          ( read $ xs !! 0 :: Integer
          , fmap (read) <$> (filter (not . null))
              $ splitOn " " $ xs !! 1 :: [Integer])
        )
        $ fmap (splitOn ":") ls
  putStrLn . show . sum . fmap fst $ filter isValid $ fmap (\(x,y) -> (x, reverse y)) d
  putStrLn . show . sum . fmap fst $ filter isValid' d
