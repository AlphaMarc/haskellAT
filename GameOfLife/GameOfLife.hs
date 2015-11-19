module GameOfLife where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay)
import Control.Applicative ((<*>))
import Data.List (nub)


data Cell = Dead | Alive deriving (Show, Eq)

type Grid = [(Int,Int)]


maxX = 20
maxY = 20

initGrid :: Grid
initGrid = [(3,3), (3,4), (4,3), (4,4)]

glider :: Grid
glider = [(4,3), (5,4), (3,5), (4,5), (5,5)]

glider2 :: Grid
glider2 = [(2,2),(3,3),(3,4),(4,2),(4,3)]

glider3 :: Grid
glider3 = [(2,3),(4,4),(3,4),(4,2),(4,3)]


-- | Returns the next state of a Cell for a given number of neighbours
--
-- >>> map (nextCellState Dead) [1..8]
-- [Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead]
-- >>> map (nextCellState Alive) [1..8]
-- [Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead]
nextCellState :: Cell -> Int -> Cell
nextCellState Dead a  = if (a <= 2 || a > 3) then Dead else Alive
nextCellState Alive a = if (a < 2 || a > 3)  then Dead else Alive

nextState :: Grid -> Grid
-- check amongst the living cells, add cells that have 2 or 3 neighbours
nextState g = filterTheLiving g ++ summonTheDead
	           where filterTheLiving = filter (\e -> nextCellState Alive (numberOfNeighbours e g) == Alive)
	                 summonTheDead = filter (\e-> (not (e `elem` g)) && nextCellState Dead (numberOfNeighbours e g) == Alive) (cellsOfInterest g)


cellsOfInterest :: Grid -> Grid
cellsOfInterest (g:gs) = nub $ generateNeighbours g ++ cellsOfInterest gs
cellsOfInterest [] = []
                         

generateNeighbours :: (Int,Int) -> Grid
generateNeighbours (i,j) = filter (\(k,l) -> (k >= 0 && l >= 0)) $
	                        [(\(a,b) -> (a+1,b-1)),
                            (\(a,b) -> (a+1,b+0)), 
                            (\(a,b) -> (a+1,b+1)), 
                            (\(a,b) -> (a+0,b+1)),
                            (\(a,b) -> (a+0,b-1)),
                            (\(a,b) -> (a-1,b+1)),
                            (\(a,b) -> (a-1,b+0)),
                            (\(a,b) -> (a-1,b-1))] <*> [(i,j)]



-- | for a given position and a Grid, returns the number of neighbours alive
numberOfNeighbours :: (Int, Int) -> Grid -> Int
numberOfNeighbours (a,b) = length . filter (\(i,j) -> if (a==i && b==j) then False else abs (i-a) <= 1 && abs (j-b) <= 1)


showGrid :: Grid -> String
showGrid g = foldr (\a b -> a++"\n"++b) ("") (reverse (f maxY)) 
             where f 0 = []
             	   f k = (generateLine (livingPosOnLine k g)) : (f (k-1))

                 
livingPosOnLine :: Int -> Grid  -> [Int]
livingPosOnLine i = map snd . filter (\(a,_) -> a == i) 


generateLine :: [Int] -> [Char]
generateLine l = map (f l) [1..maxX]
                 where f l e = if e `elem` l then '#' else ' ' 


gameOfLife :: Grid -> [Grid]
gameOfLife = iterate nextState  


-- #######################################

type Frame = [Char]
type Animation = [Frame]

displayFrame :: Frame -> IO ()
displayFrame = putStrLn

timeAction :: IO () -> IO Integer
timeAction act = do t <- (round `fmap` getPOSIXTime)
                    act
                    t' <- (round `fmap` getPOSIXTime)
                    return (fromIntegral $ t' - t)

addDelay :: Integer -> IO () -> IO ()
addDelay hz act = do dt <- timeAction act
                     let delay = calcDelay dt hz
                     threadDelay $ fromInteger delay

calcDelay dt hz = max (frame_usec - dt_usec) 0
  where frame_usec = 1000000 `div` hz
        dt_usec = dt * 1000

runFrames :: Integer -> Animation -> IO ()
runFrames hz frs = mapM_ (addDelay hz . displayFrame) frs




main = runFrames 10 (map showGrid (gameOfLife glider))
--main = do mapM_ (putStrLn . showGrid) (take 10 (gameOfLife glider))

run :: Int -> Grid -> IO ()
run i g = do mapM_ (putStrLn . showGrid) (take i (gameOfLife g))