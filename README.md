# Conway's Game of Life

### Build
Clone the repository and run `cabal install --dependencies-only`.

Then run `cabal run` to run with default initial state. To run with custom initial state, run `cabal build` and then run `./dist/build/gameoflife <args>` where `args` is a list of list of coordinates that should be alive in the form of `x,y`.

For example:

```
./dist/build/gameoflife 0,1 1,2 2,0 2,1 2,2
```

### Rules
- Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
- Any live cell with two or three live neighbours lives on to the next generation.
- Any live cell with more than three live neighbours dies, as if by overpopulation.
- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.


Written with [Brick](https://github.com/jtdaugherty/brick).
