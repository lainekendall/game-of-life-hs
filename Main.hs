module Main where

main :: IO ()
main = do print "Hi"

type Grid = [[Bool]]
type SmartGrid = [[(Int,Int,Bool)]]
type Cell = Bool
type SmartCell = (Int,Int,Cell)
type Coordinate = (Int,Int)

-- Turn grid from state 1 to state 2, applying rules to each cell
evolve :: Grid -> Grid
evolve grid = let 
    gridWithCoords = smartedGrid grid
    computeFunc = computeNewValue gridWithCoords
    evolvedGrid = map (map computeFunc) gridWithCoords
      in evolvedGrid

-- Turn a Grid into a SmartGrid
smartedGrid :: Grid -> SmartGrid
smartedGrid grid = [get2DCoords rowHeight (grid !! rowHeight) | rowHeight <- [0..height (Left grid) - 1]]

-- Given a rowHeight, turn a list of cells into SmartCells
get2DCoords :: Int -> [Cell] -> [SmartCell] -- enhancement: add Cell here
get2DCoords rowHeight row = [(x,rowHeight,b) | x <- [0..length row - 1], b <- [row !! x]]

-- Get height of a 2D array, assumes 2D array is square
height :: Either Grid SmartGrid -> Int
height (Left grid) = length (grid !! 0)
height (Right smartGrid) = length (smartGrid !! 0)

extractCell :: Either Cell SmartCell -> Cell
extractCell (Left c) = c;
extractCell (Right (_,_,c)) = c;

lookupCell :: Either Grid SmartGrid -> Coordinate -> Cell
lookupCell grid (x,y)
    | x < 0 || y < 0 || x >= (length grid) || y >= (height grid) = False
    | otherwise = extractCell (grid !! x) !! y

lookupSmartCell :: SmartGrid -> Coordinate -> Cell
lookupSmartCell grid (x,y)
    | x < 0 || y < 0 || x >= (length grid) || y >= (height grid) = False
    | otherwise = let (_,_,bool) = (grid !! x) !! y in bool

-- Given a SmartGrid, apply game of life rules to a SmartCell and transform it into a Cell
computeNewValue :: SmartGrid -> SmartCell -> Cell
computeNewValue smartGrid coord = applyRule aliveNeighbors alive where
    coord2 = threeToTwo coord
    aliveNeighbors = countAliveNeighbors smartGrid coord2
    alive = lookupSmartCell smartGrid coord2

threeToTwo :: (a,b,c) -> (a,b)
threeToTwo (a,b,_) = (a,b)

countAliveNeighbors :: SmartGrid -> Coordinate -> Int
countAliveNeighbors grid coord = countAliveFromCoords grid (getNeighborCoords coord)

countAliveFromCoords :: SmartGrid -> [Coordinate] -> Int
countAliveFromCoords grid coords = foldl (countCellsAtCoords grid) 0 coords

countCellsAtCoords :: SmartGrid -> Int -> (Int, Int) -> Int
countCellsAtCoords grid soFar next = let boolAtCoord = lookupSmartCell grid next
    in soFar + boolToInt boolAtCoord

boolToInt :: Cell -> Int
boolToInt True = 1
boolToInt False = 0

getNeighborCoords :: Coordinate -> [Coordinate]
getNeighborCoords (x,y) = [(x1,y1) | x1 <- [x+1,x,x-1], y1 <- [y+1,y,y-1], (x1,y1) /= (x,y)]

applyRule :: Int -> Cell -> Cell
applyRule aliveNeighbors alive =
    case aliveNeighbors of
      2 -> alive
      3 -> True
      4 -> alive
      _ -> False
