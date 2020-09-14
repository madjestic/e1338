# e1338
a re-write of e1337 with BearRiver (and, maybe, MVars)

![](https://github.com/madjestic/e1338/blob/master/output.png)

[a youtube demo](https://youtu.be/vLVI2mkBmlw)

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

* Improved camera controls.
* Project file generator.
* Speed controls for keyboard movement.
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
$ ./convertGeo.sh earth 
```

## Creating a new Material:
```bash
$ ./cabal run genMaterial mat/testMat01
```
(that generates a ./mat/testMat01 material directory with a default conent (constant shader material)

## Working with REPL:
- compiling:
```bash
$ cabal repl exe:e1338
```





