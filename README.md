## Running:
Run the program with:

```bash
cabal run
```

## Testing:
The `solve` function takes the options and board structs and return a completed
board if a solution exists.

To create a board use the `newBoard` function passing the options struct as parameter.

The Options struct is defined as:

```haskell
Options n maxHeight viewReq checkDiags
```

Where `n` is the size of the board, and `checkDiags` is whether the game should check
to see if there are repeating building heights in the two diagonals.

The view requirements are given as the struct:

```haskell
ViewReq rowReqs colReqs
```

Where both values are vectors of `(Int, Int)`, the first number of the tuple is the
requirement on the start of the line, and the second one, the end of the line.
The requirements are read top to bottom and left to right, so the following:

```haskell
viewReq = ViewReq (Vector.fromList [(1, 3), (2, 2), (0, 1)]) (Vector.fromList [(0, 0), (2, 2), (3, 1)])
opts = Options 3 3 viewReq False
board = newBoard opts
```

translates to:

```
       2 3
   1 . . . 3
   2 . . . 2
     . . . 1
       2 1
```
