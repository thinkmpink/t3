# t3

### Tic Tac Toe in Haskell

Run and build this like any Haskell program. You can start entering input as soon as you run the program.

T3 is based on commands like `showBoard`. There also are commands like `addPlayer <username> <mark>`, `pickSpot <column> <row>`, and `setBoardSize <width>` that modify the state of the board.

While the program will by default take input from stdin and print output to stdout, you can save all your commands to a file `game.txt`, separated by newlines, and run the program like this:
```
$ stack exec t3-exe < game.txt > game-output.txt
```

Have fun!
