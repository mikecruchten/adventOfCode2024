import System.IO
import Text.Regex.TDFA

mulRegEx :: String
mulRegEx = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

mulDoRegEx :: String
mulDoRegEx = mulRegEx ++ "|do\\(\\)|don't\\(\\)"

filteredUsing :: String -> String -> [String]
filteredUsing s m = filter (not . null) $ helper s []
  where helper [] r = r
        helper t  r =
          let
            (pre,found,post) = (t =~ m) :: (String,String,String)
          in
            helper post (found : r)

multiplyMul :: String -> Int
multiplyMul s = n1 * n2
  where n1 = (read . takeWhile (/= ',') $ drop 4 s) :: Int
        n2 = (read . takeWhile (/= ')') . (drop 1)
          $ dropWhile (/= ',') s) :: Int

multiplyDoMuls :: [String] -> Bool -> Int
multiplyDoMuls []       _ = 0
multiplyDoMuls (x : xs) b | x == "do()"    = multiplyDoMuls xs True
                          | x == "don't()" = multiplyDoMuls xs False
                          | b              = multiplyMul x
                              + multiplyDoMuls xs b
                          | otherwise      = multiplyDoMuls xs b

main :: IO()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let
    foundMuls = contents `filteredUsing` mulRegEx
    foundMulDos = reverse $ contents  `filteredUsing` mulDoRegEx
    result = sum (fmap multiplyMul foundMuls)
    result2 = multiplyDoMuls foundMulDos True
  putStrLn $ show result
  putStrLn $ show result2
