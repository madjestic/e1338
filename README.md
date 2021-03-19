# e1338
a re-write of e1337 with BearRiver (and, maybe, MVars)

![](https://github.com/madjestic/e1338/blob/master/output.png)

[a youtube demo](https://youtu.be/UwSk4vkb3-Y)

clone repo, `cabal build`, `./run.sh`

```
w,a,s,d,q,e - 6-axis movement
PgUp/PgDown - roll left/right
arrow keys, mouse controls - yaw, pitch
LCtrl          - slow
LShift         - fast
LCtrl + LShift - very fast
C              - switch camera

Space - reset

Esc - quit
```

## Recent Progress:

* Basic shading and animations.
* Mercury, Venus, Earth (Moon), Mars, Jupiter.
* Project file supports multiple cameras with individual params.
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
$ cabal run genMaterial matDir/matName
```
(that generates a ./mat/testMat01 material directory with a default conent (constant shader material)

## Generate a new Project:
```bash
$ cabal run exe:genProject -- ./projects/test
```

## Fix project's UUIDs:
```bash
$ cabal run exe:genUUID -- -p ./projects/test
```

## Fix material's UUIDs:
```bash
$ cabal run exe:genUUID -- -m mat/test/test
```

## Working with REPL:
- compiling:
```bash
$ cabal repl exe:e1338
```





