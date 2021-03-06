module Life_backend where

import System.Random
import Data.List

-- Haskell Implementation of Conway's Game of Life with probability
-- Cells change state based on the set probability

type Position = (Float,Float)

data CellState = Alive | Dead
    deriving (Eq, Show)

data Cell = Cell CellState Position
    deriving (Eq, Show)

type Board = [Cell]

data Result = EndOfGame Board       -- end of game, everything is dead
            | ContinueGame Board    -- continue with updated board


-- Get all the internal postions from a list of cells
getCellPositions :: [Cell] -> [Position]
getCellPositions cells = [position | (Cell state position) <- cells]

-- tests
-- let position = (0,0)
-- let cell = Cell Alive position
-- let cells = [cell]
-- getCellPositions cells
-- let cells = [cell, cell, cell]
-- getCellPositions cells


-- (-1,-1) (0, -1) (1, -1)
-- (-1, 0)         (1 , 0) <- relative adjacent coordinates
-- (-1, 1) (0 , 1) (1 , 1)

-- get the world coordinates of adjacent cells to a given position
getAdjacents :: Position -> [Position]
getAdjacents (x,y) = map (\ (i,j) -> (i+x, j+y)) adjacent
    where adjacent = [(-1,-1), (0, -1), (1, -1), (-1, 0), (1 , 0), (-1, 1), (0 , 1), (1 , 1)]

-- tests
-- getAdjacents (0,0)
-- getAdjacents (3,4)


-- Given a Cell and the Board it resides on, return a list of it's neighbouring Cells
neighbours :: Cell -> Board -> [Cell]
neighbours (Cell _ position) board =
    currAdjacents ++ otherAdjacents
    where
        adjacentPositions = getAdjacents position
        currAdjacents = [(Cell state position) | (Cell state position) <- board,  position `elem` adjacentPositions] -- Already existing cells on board
        otherAdjacents = [(Cell Dead position) | position <- adjacentPositions, not (position `elem` (getCellPositions currAdjacents))] -- Cells currently not on the board

-- tests
-- let cell1 = Cell Alive (3,3)
-- let cell2 = Cell Alive (3,4)
-- let board = [cell1, cell2]
-- neighbours cell1 board

-- Given a cell and the board, generates the positions where new cells are to be made
getNewNeighbours :: Cell -> [Cell] -> [Position]
getNewNeighbours (Cell _ position) board = otherAdjacents
    where
        adjacentPositions = getAdjacents position
        currAdjacents = [position | (Cell state position) <- board,  position `elem` adjacentPositions] -- Already existing postions of cells on board
        otherAdjacents = [position | position <- adjacentPositions, not (position `elem` currAdjacents)] -- Postions of cells currently not on the board

-- Takes a board, and generates all possible positions for new cells
getNewCellPositions :: Board -> [Position]
getNewCellPositions board = refine [getNewNeighbours (Cell state position) board | (Cell state position) <- board, state == Alive]
    where 
        refine lst = nub (concat lst)

-- Makes new dead cells at the given positions and adds them to the board
makeNewCells :: [Position] -> Board -> Board
makeNewCells positions board = foldr (:) board [(Cell Dead pos) | pos <- positions]

-- returns True if cell is Alive
isAlive :: Cell -> Bool
isAlive (Cell state _)
    | state == Alive = True
    | otherwise = False

-- Counts the number of occurances in the list that matches parameter p
count :: Num p => (t -> Bool) -> [t] -> p
count p [] = 0
count p (h:t) = if p h then 1 + count p t else count p t

-- Generates the next cell state given the cell, its neighbours and a probability (0-100)
-- A cell with 2 or 3 live neighbours survives
-- A dead cell with 3 live neighbours becomes live
-- All other cells become dead. Dead cells stay dead
nextCellGenP :: Cell -> [Cell] -> Int -> Cell
nextCellGenP (Cell state position) nb prob = if state == Alive then stillAlive (Cell Alive position) nb prob else stillDead (Cell Dead position) nb prob

-- Determines if a living cell remains alive
-- A cell with 2 or 3 living neighbours remains alive, otherwise the cell dies with the given probability
stillAlive :: Cell -> [Cell] -> Int -> Cell
stillAlive (Cell state position) nb prob = if ((count (==True) (map isAlive nb)) `elem` [2,3]) then (Cell Alive position) else switch (Cell Alive position) prob

-- Determines if a dead cell remains dead
-- A cell with 3 living neighbours becomes alive with given probability, otherwise the cell remains dead
stillDead :: Cell -> [Cell] -> Int -> Cell
stillDead (Cell state position) nb prob = if ((count (==True) (map isAlive nb)) == 3) then switch (Cell Dead position) prob else (Cell Dead position)

-- Based on the probability given, the cell changes state. Otherwise the cell remains the same
switch :: Cell -> Int -> Cell
switch (Cell state position) a = if ((state == Alive && a == 1) || (state == Dead && a == 0)) then (Cell Dead position) else (Cell Alive position)


-- Given a probability, p (1-100), generates a list with p 1's and (100-p) 0's and returns the value at the given index
pick :: Num a => Int -> Int -> a
pick p index = (replicate p 1 ++ (replicate (100-p) 0)) !! index

--tests
-- c1 = Cell Alive (0,1)
-- c2 = Cell Alive (1,0)
-- c3 = Cell Alive (1,1)
-- c4 = Cell Alive (1,2)
-- c5 = Cell Alive (2,1)
-- c6 = Cell Dead (2,2)
-- board = [c1,c2,c3,c4,c5,c6]
-- nb = neighbours c3 board
-- neigh = neighbours c6 board

-- Generates next board with a given probability and an infinite list of random numbers in the range (0-99)
nextBoardGen :: Board -> Int -> [Int] -> Board
nextBoardGen board prob randList = [nextCellGenP cell (neighbours cell board) (pick prob (randList !! i)) | (i, cell) <- (zip [0..] board)]


-- Removes all dead cells from the board
removeDeadCells :: Board -> Board
removeDeadCells board = [ (Cell state position) | (Cell state position) <- board, state == Alive ]


-- Returns true if all cells on the board are dead, else false
allDead :: Board -> Bool
allDead board = all (==Dead) [state | (Cell state position) <- board]

-- tests
-- let cell1 = Cell Alive (3,3)
-- let cell2 = Cell Alive (3,3)
-- let board = [cell1, cell2]
-- allDead board
-- let board = [cell1, cell1]
-- allDead board
-- allDead []

-- Calculates the next board 
-- If all cells are dead, returns EndGame, otherwise, returns the board as part a of ContinueGame
gameOfLife :: Board -> Int -> IO Board
gameOfLife board p =
    do
        rg <- newStdGen
        let randList = randomRs (0,99) rg
            newCellPositions = getNewCellPositions board
            filledBoard = makeNewCells newCellPositions board
            nextBoard = removeDeadCells (nextBoardGen filledBoard p randList)
        putStrLn(show filledBoard)
        return nextBoard
        
--test case
--let cell1 = Cell Alive (3,3)
--let cell2 = Cell Alive (3,4)
--let cell3 = Cell Alive (3,5)
--let board = [cell1, cell2, cell3]
--gameOfLife board 100
