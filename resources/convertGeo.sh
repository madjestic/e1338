#!/bin/sh

# a wrapper for an otherwise lengthy expression
# @echo "converting $1 to $2 and copying to $3"

# SOURCE="./models/$1.geo"
SOURCE="./resources/models/$1.geo"
TARGET="./models/$1.bgeo"
PDG="./resources/models/$1.pgeo"
PDGCPY="./models/$1.pgeo"
INDEX=${2:-} # optional argument {--skip} or nothing {}. e.g. foo --skip

convertGeo(){
    if [ -e "$SOURCE" ]
    then
	echo "Running geoParser.py..."
	echo "python ./resources/geoParser.py" $SOURCE $PDG $INDEX
	python ./resources/geoParser.py $SOURCE $PDG $INDEX
	echo "Running geoIndexer..."
	echo "cabal run geoIndexer" $PDG $TARGET $INDEX
	cabal run geoIndexer -- -i $PDG -o $TARGET $INDEX
	echo "Copying files..."
	echo "cp" $PDG $PDGCPY
	# cp $PDG $PDGCPY
    else
	echo "pass"
	# python ./resources/geoParser.py
	# cabal run geoIndexer ./models/model.geo ../models/model.bgeo
	# cp ./models/model.geo ../models/model.bgeo
    fi
}

convertGeo $1

# ./convertGeo.sh ./models/earth.geo ./models/earth.pgeo ../models/earth.bgeo
# ./convertGeo.sh earth
