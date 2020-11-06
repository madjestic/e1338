# e1338
a re-write of e1337 with BearRiver (and, maybe, MVars)

![](https://github.com/madjestic/e1338/blob/master/output.png)

[a youtube demo](https://www.youtube.com/watch?v=W8S1ju8DYME)

clone repo, `cabal build`, `./run.sh`

```
w,a,s,d,z,c - 6-axis movement
qe - roll
arrow keys, mouse controls - yaw, pitch
LCtrl          - slow
LShift         - fast
LCtrl + LShift - very fast

Space - reset

Esc - quit
```

## Recent Progress:

* Logarithmic Space (rendering objects in real scale at 10 billion kilometers is ok).
* Improved camera controls.
* Project file generator.
* Speed controls for keyboard movement.
* Foreground and Background objects with different rendering options.
+ Working on basic solvers (spinners, lodders, physics).

## Building and running:
```bash
$ cabal build
$ gpu ./run.sh
```
or, if optirun works for you:
```
$ optirun ./rungl.hs
```

## Convert a Geo Model:
```bash
$ ./resources/convertGeo.sh earth
```

## Creating a new Material:
```bash
$ cabal run genMaterial mat/testMat01
```
(that generates a ./mat/testMat01 material directory with a default conent (constant shader material)

## Generate a new Project:
```bash
$ cabal run genProject ./projects/test
```

## Working with REPL:
- compiling:
```bash
$ cabal repl exe:e1338
```





