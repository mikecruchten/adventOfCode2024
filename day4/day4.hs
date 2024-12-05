import System.IO
import Text.Regex.TDFA

import Data.List

type Row = String
type Col = String
type Table = [Row]

instance {-#OVERLAPPING #-} Show Table where
  show t = intercalate "\n" t

-- rotate a row by some integer
-- we give back the remainder of the rotation and the
-- overlapping parts separately as the puzzle does
-- not look at words which cross the boundary
rotateRow :: Row -> Int -> (Row,Row)
rotateRow r n | n >= 0    = ( replicate n '.' ++ (take (len - n) r)
                            , drop (len - n) r ++ replicate (len - n) '.'
                            )
              | otherwise = ( drop (-n) r ++ replicate (-n) '.'
                            , replicate (len + n) '.' ++ take (-n) r
                            )
  where len = length r

-- compute 4 tables, of which each line corresponds
-- to one diagonal of the table
diagonals :: Table -> ((Table,Table),(Table,Table))
diagonals t = (lTranspose $ diagonal 1, lTranspose $ diagonal (-1))
  where dim = length t
        diagonal d = foldr
          (\(r1,r2) (t1,t2) -> (r1:t1,r2:t2))
          ([],[])
          [ rotateRow (t !! i) (d * i) | i <- [0 .. dim - 1] ]
        lTranspose (t1,t2) = (transpose t1,transpose t2)

-- count the number of XMASs found in the table when
-- reading from left to right
tableXmasCount :: Table -> Int
tableXmasCount []       = 0
tableXmasCount (x : xs) =
  let
    (pre,found,post) = (x =~ "XMAS") :: (String,String,String)
  in
    case found of
      "" -> tableXmasCount xs
      _  -> 1 + tableXmasCount (post : xs)

-- filter a row, by replacing all chars by '.'
-- apart from those As which are a part of MAS or SAM
process :: String -> String
process [] = []
process s  =
  let
    (pre,found,post) = (s =~ "MAS|SAM") :: (String,String,String)
  in
    case found of
      "" -> ['.' | _ <- [1 .. length s]]
      _  -> ['.' | _ <- [1 .. length pre]]
         ++ ".A"
         ++ process (last found : post)

-- reverse a table
revTable :: Table -> Table
revTable t = reverse $ fmap reverse t

-- count how often XMAS occurred in the table,
-- reading in all possible directions
xMasCount :: Table -> Int
xMasCount t = sum . (fmap tableXmasCount)
  $   [id, revTable]
  <*> [ t, transpose t, diag1, diag2, diag3, diag4 ]
  where ((diag1, diag2), (diag3, diag4)) = diagonals t

-- given a position function pf and a table,
-- compute the positions (in the original table, using
-- pf) of As which are part of MAS or SAM
validAPos :: (Int -> Int -> (Int,Int)) -> Table -> [(Int,Int)]
validAPos pf t = do j <- [0 .. dim - 1]
                    i <- [0 .. dim - 1]
                    if (processedT !! j) !! i == 'A'
                      then return $ pf i j
                      else []
  where dim = length t
        processedT = fmap process t

-- check if an element occurs more often than once in a table
multiOccurs :: Eq a => [a] -> [a]
multiOccurs [] = []
multiOccurs (x : xs) | x `elem` xs = x : multiOccurs xs
                     | otherwise   = multiOccurs xs

-- compute all positions of As that lie on two
-- diagonals
allValidAPos :: Table -> [(Int,Int)]
allValidAPos t = nub . multiOccurs 
  $ concat [ validAPos (\i j -> (i, j - i)) d1
           , validAPos (\i j -> (i, j - i + dim)) d2
           , validAPos (\i j -> (i, i + j)) d3
           , validAPos (\i j -> (i, j + i - dim)) d4
           ]
  where dim = length t
        ((d1,d2),(d3,d4)) = diagonals t

crossMasCount :: Table -> Int
crossMasCount t = length $ allValidAPos t

main :: IO()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  putStrLn . show $ xMasCount ls
  putStrLn . show $ crossMasCount ls
