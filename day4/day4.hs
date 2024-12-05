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

-- merging the diagonal pieces back into one table
-- if ((d1,d2),(d3,d4)) are the diagonals computed via diagonals,
-- then mergeRows (transpose d1, transpose d2) and
--      mergeRows (transpose d4, transpose d3) both
-- yield the initial table
mergeRows :: (Table,Table) -> Table
mergeRows ((r1 : r1s),(r2 : r2s)) =
  ((dropWhile (== '.') r1) ++ (takeWhile (/= '.') r2))
  : mergeRows (r1s,r2s)
mergeRows _                       = []
  
-- checking if two rows agree on the positions of A
overlap :: (Row,Row) -> Int
overlap (rw1,rw2) = foldl
  (\r (r1,r2) -> if (r1,r2) == ('A','A')
                    then r + 1
                    else r
  )
  0
  $ zip rw1 rw2

-- filter a row, by replacing all chars by '.'
-- apart from those As which are a part of MAS or SAM
process :: String -> String
process [] = []
process s  =
  let
    (pre,found,post) = (s =~ "MAS|SAM") :: (String,String,String)
  in
    case found of
      "" -> fmap (\c -> if c == '.' then '.' else '?') s
      _  -> fmap (\c -> if c == '.' then '.' else '?') pre
         ++ "?A"
         ++ process (last found : post)

crossMasCount :: Table -> Int
crossMasCount t = foldl (\r d -> r + overlap d) 0 $ zip ds1 ds2
  where ((d1,d2),(d3,d4)) = diagonals t
        ds1 = mergeRows ( transpose $ process <$> d1
                        , transpose $ process <$> d2
                        )
        ds2 = mergeRows ( transpose $ process <$> d4
                        , transpose $ process <$> d3
                        )

main :: IO()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  putStrLn . show $ xMasCount ls
  putStrLn . show $ crossMasCount ls
