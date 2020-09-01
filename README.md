# e1338
a re-write of e1337 with BearRiver (and, maybe, MVars)

![](https://github.com/madjestic/e1338/blob/master/output.png)

[a youtube demo](https://youtu.be/vLVI2mkBmlw)

clone repo, `cabal build`,

wasd, qe, zc, arrow keys, mouse controls

space - reset

Esc - quit

## Recent Progress:

* Added mad camera.
+ Workgin on basic solvers (spinners).

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
$ cabal repl exe:e1337
```





