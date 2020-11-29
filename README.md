# ReverSync -- A genetically-trained evaluation function for Reversi (Othello) in LISP

The goal is to create an evaluation function used in a MiniMax algorithm with alpha-beta pruning to win at the board game [reversi](https://es.wikipedia.org/wiki/Reversi), also known as Othello.

The project is written in LISP and makes use of [Peter Norvig](https://github.com/norvig)'s reversi LISP package to set the game up and run the MiniMax algorithm.

## Playing against the heuristic

If you want to play against the machine, you can do so by running the files _playAsWhite.lisp_ or _playAsBlack.lisp_ using a lisp interpreter such as _clisp_. Doing so will load the library
and the heuristic, and start the game.

## Overview of the evaluation function

The evaluation function accepts the state of the board and the current player, and returns a value that indicates whether the current player has a favorable position against its opponent. The 
output is a floating point value that can either be positive or negative (player winning vs player losing), and whose value indicates by how much the player is winning/losing. This value is then used
by the algorithm to select the best move.

The function analyses different features of the board, and then assigns weights to each of the features. The weights are optimized using a genetic algorithm.

More information on the evaluation function can be found on the project's [wiki](https://github.com/MiguelGonzalez2/reversync/wiki).

## Genetic algorithm to fit the weights

This algorithm starts by assigning random weights (genome) to several copies of the evaluation function (specimens). They are then matched with several test evaluation functions, and those who perform
best are selected to be _bred_, that is, their genomes are mixed to obtain the next generation. This is iterated until decided, and the weights of the last generation are used.

The algorithm can be run on reversync via executing _genetic-trainer.lisp_. This will do 5 iterations of the algorithm, printing the fitnesses of each specimen on each iteration. The file
has a header with several parameters that can be tweaked to adjust the training, and a different evaluation function can be trained as long as it follows the same prototype as explained on the file.

_Caution: the process takes a long time to run, since it plays several games per specimen and games are played among all specimens._ 
