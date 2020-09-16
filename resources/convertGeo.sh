#!/bin/sh

# a wrapper for an otherwise lengthy expression
# @echo "converting $1 to $2 and copying to $3"

# SOURCE="./models/$1.geo"
SOURCE="./resources/models/$1.geo"
TARGET="./models/$1.bgeo"
PDG="./resources/models/$1.pgeo"
PDGCPY="./models/$1.pgeo"

convertGeo(){
    if [ -e "$SOURCE" ]
    then
	python ./resources/geoParser.py $SOURCE $PDG
	cabal run geoIndexer $PDG $TARGET
	cp $PDG $PDGCPY
    else
	python ./resources/geoParser.py
	cabal run geoIndexer ./models/model.geo ../models/model.bgeo
	cp ./models/model.geo ../models/model.bgeo
    fi
}

convertGeo $1

# ./convertGeo.sh ./models/earth.geo ./models/earth.pgeo ../models/earth.bgeo
# ./convertGeo.sh earth
