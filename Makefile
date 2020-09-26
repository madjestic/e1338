all:
	cabal build && gpu ./run.sh

run:
	gpu ./run.sh

prof:
	cabal clean
	mvlink e1338.cabal e1338.cabal.prof
	mvlink run.sh run.sh.prof
	cabal build

current:
	cabal clean
	mvlink e1338.cabal e1338.cabal.current
	mvlink run.sh run.sh.current
	cabal build

