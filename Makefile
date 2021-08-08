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
	./resources/convertGeo.sh fnt_a
	./resources/convertGeo.sh fnt_b
	./resources/convertGeo.sh fnt_c
	./resources/convertGeo.sh fnt_d
	./resources/convertGeo.sh fnt_e
	./resources/convertGeo.sh fnt_f
	./resources/convertGeo.sh fnt_g
	./resources/convertGeo.sh fnt_h
	./resources/convertGeo.sh fnt_i
	./resources/convertGeo.sh fnt_j
	./resources/convertGeo.sh fnt_k
	./resources/convertGeo.sh fnt_l
	./resources/convertGeo.sh fnt_m
	./resources/convertGeo.sh fnt_n
	./resources/convertGeo.sh fnt_o
	./resources/convertGeo.sh fnt_p
	./resources/convertGeo.sh fnt_q
	./resources/convertGeo.sh fnt_r
	./resources/convertGeo.sh fnt_s
	./resources/convertGeo.sh fnt_t
	./resources/convertGeo.sh fnt_u
	./resources/convertGeo.sh fnt_v
	./resources/convertGeo.sh fnt_w
	./resources/convertGeo.sh fnt_x
	./resources/convertGeo.sh fnt_y
	./resources/convertGeo.sh fnt_z
	./resources/convertGeo.sh fnt_plus
	./resources/convertGeo.sh fnt_minus
	./resources/convertGeo.sh fnt_equal
	./resources/convertGeo.sh fnt_GT
	./resources/convertGeo.sh fnt_comma
	./resources/convertGeo.sh fnt_dot
	./resources/convertGeo.sh fnt_question
	./resources/convertGeo.sh fnt_exclam
	./resources/convertGeo.sh fnt_asterics
	./resources/convertGeo.sh fnt_slash
	./resources/convertGeo.sh fnt_semicolon
	./resources/convertGeo.sh fnt_quote
	./resources/convertGeo.sh fnt_space

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
#	./resources/convertGeo.sh stars --skip
	./resources/convertGeo.sh star_sector_00 --skip
	./resources/convertGeo.sh star_sector_01 --skip
	./resources/convertGeo.sh star_sector_02 --skip
	./resources/convertGeo.sh star_sector_03 --skip
	./resources/convertGeo.sh star_sector_04 --skip
	./resources/convertGeo.sh star_sector_05 --skip
	./resources/convertGeo.sh star_sector_06 --skip
	./resources/convertGeo.sh star_sector_07 --skip
	./resources/convertGeo.sh star_sector_08 --skip
	./resources/convertGeo.sh star_sector_09 --skip

3bodies:
	./resources/convertGeo.sh body_0
	./resources/convertGeo.sh body_1
	./resources/convertGeo.sh body_2
