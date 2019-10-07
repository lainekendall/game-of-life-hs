module Main where

main :: IO ()
main = do print "Hi"

type Grid = [[Cell]]
type SmartGrid = [[SmartCell]]
type Cell = Bool
type SmartCell = (Int,Int,Cell)
type Coordinate = (Int,Int)

-- Turn Grid from state 1 to state 2, applying rules to each cell
evolve :: Grid -> Grid
evolve grid = let 
    smartGrid = smartenGrid grid
    computeFunc = computeNewValue smartGrid
    in map (map computeFunc) smartGrid

-- Turn a Grid into a SmartGrid
smartenGrid :: Grid -> SmartGrid
smartenGrid grid = [smartenRow rowHeight (grid !! rowHeight) | rowHeight <- [0..height grid - 1]]

-- Given a rowHeight, turn a list of Cells into SmartCells
smartenRow :: Int -> [Cell] -> [SmartCell]
smartenRow rowHeight row = [(x,rowHeight,b) | x <- [0..length row - 1], b <- [row !! x]]

-- Given a SmartGrid, apply game of life rules to a SmartCell and transform it into a Cell
computeNewValue :: SmartGrid -> SmartCell -> Cell
computeNewValue smartGrid coord = applyRule aliveNeighbors alive where
    coord2 = threeToTwo coord
    aliveNeighbors = countAliveNeighbors smartGrid coord2
    alive = lookupCoord smartGrid coord2

-- Drop third element of a 3-tuple
threeToTwo :: (a,b,c) -> (a,b)
threeToTwo (a,b,_) = (a,b)

-- Given a SmartGrid and a Coordinate, count how many alive neighbors that cell has
countAliveNeighbors :: SmartGrid -> Coordinate -> Int
countAliveNeighbors grid coord = countAliveFromCoords grid (getNeighborCoords coord)

-- Given a coordinate, get all 8 neighbors coordinates
getNeighborCoords :: Coordinate -> [Coordinate]
getNeighborCoords (x,y) = [(x1,y1) | x1 <- [x-1..x+1], y1 <- [y-1..y+1], (x1,y1) /= (x,y)]

-- Given a SmartGrid and a list of Coordinates, count how many of the list are alive
countAliveFromCoords :: SmartGrid -> [Coordinate] -> Int
countAliveFromCoords grid coords = foldl (foldCells grid) 0 coords

-- Fold function for summing up Cells aliveness
foldCells :: SmartGrid -> Int -> Coordinate -> Int
foldCells smartGrid soFar next = let 
    boolAtCoord = lookupCoord smartGrid next
      in soFar + fromEnum boolAtCoord

-- Apply Conway Game of Life Rules to a Cell, given number of alive neighbors and the cells state
applyRule :: Int -> Cell -> Cell
applyRule aliveNeighbors alive =
    case aliveNeighbors of
      2 -> alive
      3 -> True
      4 -> alive
      _ -> False

-- Grid utility functions
height :: [[a]] -> Int
height g = length (g !! 0)

lookupCoord :: SmartGrid -> Coordinate -> Bool
lookupCoord smartGrid (x,y) = let 
  (_,_,alive) = (smartGrid !! x) !! y 
    in checkOutOfBounds smartGrid (x,y) && alive

-- Return False if coordinate is out of bounds for the grid
checkOutOfBounds :: [a] -> Coordinate -> Bool 
checkOutOfBounds grid (x,y) = elem x valid && elem y valid where
  valid = [0,length grid - 1]


