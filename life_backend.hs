module Life_backend where

-- currently I worry we might have a problem where the Board will get too big
-- some solutions could be: - have a set width and height of the board - not a huge fan since the board should be infinite in the game of life
--                          - we trim it when an area is all "dead" - I like this one more and we can probably implement it later rather than sooner

type Position = (Int,Int)

data CellState = Alive | Dead deriving Eq

data Cell = Cell CellState Position

type Board = [Cell]

data Result = EndOfGame             -- end of game, everything is dead - TODO: figure how to represent
            | ContinueGame Board    -- continue with updated board


-- get all the internal postions from a list of cells
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


-- given a Cell and the Board it resides on, return a list of it's neighbouring Cells
-- currently buggy because if a cell doesn't exist in the board yet then we won't return it
neighbours :: Cell -> Board -> [Cell]
neighbours (Cell _ (x,y)) board = 
    currAdjacents ++ [(Cell Dead position) | position <- adjacentPositions, not (position `elem` (getCellPositions currAdjacents))]
    where 
        adjacentPositions = getAdjacents (x,y)
        currAdjacents = [(Cell state position) | (Cell state position) <- board,  position `elem` adjacentPositions]
    
    
-- given a cell and it's neighbours, return the cell in the next generation
-- nextCellGeneration :: Cell -> [Cell] -> Cell

-- if all dead then return EndOfGame
-- if not then calculate the next board and return it as part of ContinueGame
-- gameOfLife :: Board -> Result

