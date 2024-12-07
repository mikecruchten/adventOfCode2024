import System.IO
import Data.List
import Data.Map hiding (filter)

data Grid = Grid { unGrid :: [String]
                 , rows :: Int
                 , cols :: Int
                 }
          deriving (Eq, Ord)

instance Show Grid where
  show g = intercalate "\n" (unGrid g)

showTravel :: Guard -> Grid -> Grid
showTravel guard g = g {
  unGrid = do i <- [0 .. rows g - 1]
              return $ do
                j <- [0 .. cols g - 1]
                if (i,j) `elem` (pos guard)
                  then return . represent $ (crossType guard) ! (i,j)
                  else return (((unGrid g) !! i) !! j)
  }
  where represent (x1,x2,x3,x4) | (x1 + x3 >= 1) &&
                                  (x2 + x4 == 0) = '-'
                                | (x2 + x4 >= 1) &&
                                  (x1 + x3 == 0) = '|'
                                | otherwise      = '+'

data Direction = LEFT | RIGHT | UP | DOWN
               deriving (Eq, Ord, Bounded, Enum, Show)

data Guard = Guard { getX :: Int
                   , getY :: Int
                   , dir :: Direction
                   , pos :: [(Int,Int)]
                   , crossType :: Map (Int,Int) (Int,Int,Int,Int)
                   }

instance Show Guard where
  show (Guard x y d p _) = (show x) ++ ";" ++ (show y)

findElement :: Grid -> Char -> (Int,Int)
findElement (Grid [] _ _) _ = error "empty grid"
findElement (Grid g  r c) d = (
  do i <- [0 .. r - 1]
     j <- [0 .. c - 1]
     if (g !! i) !! j == d
      then return (i,j)
      else []
   ) !! 0

get :: Grid -> Int -> Int -> Maybe Char
get (Grid g r c) i j | (i < 0)
                        || (j < 0)
                        || (i >= r)
                        || (j >= c) = Nothing
                     | otherwise    = Just ((g !! i) !! j)

set :: Grid -> Int -> Int -> Char -> Grid
set g x y c = g {
  unGrid = do i <- [0 .. rows g - 1]
              return $ do
                j <- [0 .. cols g - 1]
                if (i,j) == (x,y)
                  then return c
                  else return (((unGrid g) !! i) !! j)
  }

nextPosition :: Guard -> (Int,Int)
nextPosition (Guard x y LEFT  _ _) = (x, y - 1)
nextPosition (Guard x y RIGHT _ _) = (x, y + 1)
nextPosition (Guard x y UP    _ _) = (x - 1, y)
nextPosition (Guard x y DOWN  _ _) = (x + 1, y)

turnRight :: Direction -> Direction
turnRight LEFT  = UP
turnRight UP    = RIGHT
turnRight RIGHT = DOWN
turnRight DOWN  = LEFT

updateCrossType :: Map (Int, Int) (Int,Int,Int,Int)
                -> (Int, Int)
                -> (Int,Int,Int,Int)
                -> Map (Int, Int) (Int,Int,Int,Int)
updateCrossType ct xy newC = insertWith add xy newC ct
  where add (x1,x2,x3,x4) (y1,y2,y3,y4) =
          (max x1 y1, max x2 y2, max x3 y3, max x4 y4)

travel :: Guard -> Grid -> Maybe Guard
travel guard@(Guard x y d t ct) g = case (get g newX newY) of
      Nothing -> Just $ Guard x y d ((x,y) : t) (updateCrossType ct (x,y) (dirToChar d))
      Just c  -> if c == '#'
        then travel
          ( Guard
              x
              y
              (turnRight d)
              t
              (updateCrossType ct (x,y) (dirToChar d))
          )
          g
        else 
          if ((member (newX,newY) ct) && (below (dirToChar d) (ct ! (newX, newY))))
            then Nothing
            else travel
              ( Guard
                  newX
                  newY
                  d
                  ((x,y) : t)
                  (updateCrossType ct (x,y) (dirToChar d))
              )
              g
    where (newX, newY) = nextPosition guard
          dirToChar LEFT  = (1,0,0,0)
          dirToChar RIGHT = (0,0,1,0)
          dirToChar UP    = (0,1,0,0)
          dirToChar DOWN  = (0,0,0,1)
          below (x1,x2,x3,x4) (y1,y2,y3,y4) =
            (x1 <= y1) && (x2 <= y2) && (x3 <= y3) && (x4 <= y4)

isLooping :: Guard -> Grid -> Bool
isLooping gu gr = case travel gu gr of
  Nothing -> True
  Just _  -> False

findAllIsLooping :: Guard -> Grid -> [(Int,Int)]
findAllIsLooping guard grid = filter (/= (getX guard,getY guard)) $
  case positions of
    Nothing     -> []
    Just fGuard -> filter ((isLooping guard) . createGrid) (nub $ pos fGuard)
  where positions = travel guard grid
        createGrid (x,y) = grid {
          unGrid = do i <- [0 .. rows grid - 1]
                      return $ do
                        j <- [0 .. cols grid - 1]
                        if (i,j) == (x,y)
                          then return '#'
                          else return (((unGrid grid) !! i) !! j)
          }

main :: IO()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
      grid = Grid ls (length ls) (length (ls !! 0))
      (guardX,guardY) = findElement grid '^'
      guard = Guard guardX guardY UP [] empty
  putStrLn . show $ length . nub . pos <$> travel guard grid
  putStrLn . show $ length $ findAllIsLooping guard grid
