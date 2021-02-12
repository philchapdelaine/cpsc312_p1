# cpsc312_p1
About Haskell implementation of Conway's Game of Life

Game of Probable Life
Authors: Emily, Philippe, and Phoebe

What is the problem?
We will use Haskell to implement a simulation of Conway's Game of Life.

Conway's Game of Life is a zero-player game that is played on a two-dimensional square grid of cells that are either alive or dead based on the following set of rules:

Every cell interacts with its eight neighbours, which are the cells that are horizontally, vertically, or diagonally adjacent

Any live cell with fewer than two live neighbours dies, as if by underpopulation.
Any live cell with two or three live neighbours lives on to the next generation.
Any live cell with more than three live neighbours dies, as if by overpopulation.
Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
These rules, which compare the behaviour of the automaton to real life, can be condensed into the following:

Any live cell with two or three live neighbours survives.
Any dead cell with three live neighbours becomes a live cell.
All other live cells die in the next generation. Similarly, all other dead cells stay dead.
The initial pattern constitutes the seed of the system. The first generation is created by applying the above rules simultaneously to every cell in the seed; births and deaths occur simultaneously, and the discrete moment at which this happens is sometimes called a tick. Each generation is a pure function of the preceding one. The rules continue to be applied repeatedly to create further generations.

(Adapted from Conway's Game of Life Wikipedia Page)

What is the something extra?
We will be creating a UI which will allow the user to interact with the rules of the game. This will allow users to input their own rules on the fly as well as set the initial state.

We will also be creating a graphical user interface to display the gamestate.

We will take a probabilistic approach to implementing the rules of the game. For each iteration of the game, rather than determining for certain whether each cell will be alive or dead based on their neighbours, each cell will instead have a chance of being alive or dead based on its neighbours.

What did we learn from doing this?
We'll find out :)
