all:
	cabal build && gpu ./run.sh

run:
	gpu ./run.sh

prof:
	cabal clean
	mvlink cabal.project.local cabal.project.local.prof
	mvlink e1338.cabal e1338.cabal.prof
	mvlink run.sh run.sh.prof
	cabal build

current:
	cabal clean
	mvlink cabal.project.local cabal.project.local.current
	mvlink e1338.cabal e1338.cabal.current
	mvlink run.sh run.sh.current
	cabal build
fonts:
	./resources/convertGeo.sh fnt_0
	./resources/convertGeo.sh fnt_1
	./resources/convertGeo.sh fnt_2	
	./resources/convertGeo.sh fnt_3
	./resources/convertGeo.sh fnt_4
	./resources/convertGeo.sh fnt_5
	./resources/convertGeo.sh fnt_6
	./resources/convertGeo.sh fnt_7
	./resources/convertGeo.sh fnt_8
	./resources/convertGeo.sh fnt_9

intro:
	./resources/convertGeo.sh intro_square

planets:
	./resources/convertGeo.sh sun
	./resources/convertGeo.sh mercury
	./resources/convertGeo.sh venus
	./resources/convertGeo.sh earth
	./resources/convertGeo.sh moon
	./resources/convertGeo.sh mars
	./resources/convertGeo.sh jupiter

stars:
	./resources/convertGeo.sh stars --skip
