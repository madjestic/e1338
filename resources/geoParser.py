# Pre parser of geo.json (arrays)

# Use example:
# $ python ./geoParser.py

import json
import sys
import subprocess
from itertools import chain
from numpy     import argsort,array,concatenate
from datetime  import datetime
import argparse

# from itertools import izip

def rpcShuffler(arg=[], skip=False):
    print("initializing RPC process...")
    if skip:
        print ("Skipping Indexing")
        result = [[*range(len(arg))]]
    else:
        print ("Saving unShuffled data")
        with open('./resources/rpcShuffler/.p2h', 'w') as f:
            f.write(str(arg))
         
        print("calling rpcshuffler")
        subprocess.call(["cabal", "run", "rpcShuffler"])

        print("reading Shuffled data")
        with open('./resources/rpcShuffler/.h2p', 'r') as f:
            result = f.read()

    print("OK")
    
    return(result)


def concat(mlist):
    return list(chain.from_iterable(mlist))

def reorder(order_=[], source_order=[], target_order=[]):
    return (concatenate((array(order_))[array(concat(source_order)) [array([argsort(concat(target_order))])]]))

# Fix `/` in a file path
def fixPaths(vtxAttrMatDictVals):
    result = []
    for i in vtxAttrMatDictVals:
        result.append(i[1:])
    return result


def toDict (jsonFile):
    i        = iter (jsonFile)
    jsonDict = dict (zip (i, i))
    return jsonDict


def makeDict (jsonFile):
    if type(jsonFile) is list:
        return makeDict (toDict (jsonFile))
    else:
        return {jsonFile : []}

    
def sortArrayByIndex (array, indices):
    result = []
    for i in indices:
        result.append (array [i])
    return result


def restoreArrayFromIndex (array, indices): 
    result = []
    for i in indices:
        result.append (array [i])
    return result


def readJSON(fileIn):
    # in:  fileIn
    # out: jsonFile
    # fileIn  = "models/model.geo"
    # fileOut = "models/model.pgeo" # pgeo - processed geo, in JSON format

    inFile  = open(fileIn, "r")
    #outFile = open(fileOut, "w")
    jsonFile = json.loads(inFile.read())
    inFile.close()

    return jsonFile


def parseJSON(jsonFile):
    # in:  jsonFile
    # out: jsonStructure
    
    pointcount    = (jsonFile [4])
    vertexcount   = (jsonFile [6])

    jsonDict      = toDict (jsonFile)
    ###  TOPOLOGY  ###
    topology      = toDict (jsonDict ["topology"])
    indices       = topology["pointref"][1]

    ### ATTRIBUTES ###
    attrs         = toDict (jsonDict ["attributes"])

    # vertex attributes list
    vtxAttrs          = attrs ["vertexattributes"]

    ### Alpha vtx attr
    vtxAttrAlpha         = vtxAttrs [0]
    vtxAttrAlphaDict     = toDict (vtxAttrAlpha [1])
    vtxAttrAlphaDictVals = vtxAttrAlphaDict ["values"]
    #print("vtxAttrAlphaDictVals :",vtxAttrAlphaDictVals) 
    vtxAttrAlphaArrays   = toDict (vtxAttrAlphaDictVals) ["arrays"]

    ### Color
    vtxAttrCd         = vtxAttrs [1]
    vtxAttrCdDict     = toDict (vtxAttrCd [1])
    vtxAttrCdDictVals = vtxAttrCdDict ["values"]
    vtxAttrCdTuples   = toDict (vtxAttrCdDictVals) ["tuples"]    

    ### Normal
    vtxAttrN          = vtxAttrs[2]
    vtxAttrNDict      = toDict (vtxAttrN [1])
    vtxAttrNDictVals  = vtxAttrNDict ["values"]
    vtxAttrNTuples    = toDict (vtxAttrNDictVals) ["tuples"]

    # TODO : matIndices, shop_materialpath -> create a target dir, where materials should live,
    # TODO : create material placeholder, if does not exist - vertex shader, fragment shader, a list of textures
    ### Material
    vtxAttrMat         = vtxAttrs [3]
    vtxAttrMatDict     = toDict (vtxAttrMat [1])
    vtxAttrMatDictVals = vtxAttrMatDict ["strings"] # material paths strings
    vtxAttrMatIndices  = toDict (vtxAttrMatDict["indices"])["arrays"]
    # TODO : use /mat/genMaterial.hs matName (/mat/foo/bar)

    ### UV
    vtxAttrUV         = vtxAttrs [4]
    vtxAttrUVDict     = toDict (vtxAttrUV [1])
    vtxAttrUVDictVals = vtxAttrUVDict ["values"]
    vtxAttrUVTuples   = toDict (vtxAttrUVDictVals) ["tuples"]    

    ### Point Attributes
    ptAttrs           = attrs ["pointattributes"]
    
    ### Position point attr
    ptAttrP           = ptAttrs [0]
    ptAttrPDict       = toDict (ptAttrP[1])
    ptAttrPDictVals   = ptAttrPDict ["values"] # Point Attr Dictionary Values
    ptAttrPTuples     = toDict (ptAttrPDictVals) ["tuples"]

    ### Global Attributes
    globAttrs         = attrs ["globalattributes"]
    # print("globAttrs :", globAttrs)
    glAttrM           = (toDict(toDict((globAttrs[0])[1])["values"])["arrays"])    
    glAttrV           = (toDict(toDict((globAttrs[1])[1])["values"])["tuples"])    
    glAttrXform       = (toDict(toDict((globAttrs[2])[1])["values"])["tuples"])
    # print("debug :", glAttrXform)      

    # DEBUG:
    # print("Alpha: ", vtxAttrs [0])
    # print("Cd: "   , vtxAttrs [1])
    # print("N: "    , vtxAttrs [2])
    # print("mat: "  , vtxAttrs [3])
    # print("uv: "   , vtxAttrs [4])
    # print("P: "    , ptAttrs  [0])
    # print("globAttrs :", globAttrs)

    ### FORMAT JSON ###
    data    = {}

    # geoEntry = {'PGeo' : {}}
    geoEntry = {}
    data.update (geoEntry)

    # indices.reverse()
    jsonEntry = {'indices' : indices}
    #print("jsonEntry: ", jsonEntry["indices"])
    idx = jsonEntry["indices"]
    # print ("geoParser.idx :", idx)

    jsonEntry = {'material' : vtxAttrMatIndices}
    value = jsonEntry["material"]
    # print("initial value  :", value)
    value = concat(value)
    
    #
    # Shuffle indices per material
    #
    # print("DEBUG value :{}".format(value))
    value = eval(rpcShuffler(value))
    
    source_order = value
    # print("shuffled value :", value)
    # print("source_order :", source_order)
    target_order = []
    for elem in (array(source_order)): # for elem in (array(source_order, dtype=object)):
        target_order.append((array(idx)[elem]).tolist())
        #print("target_order :", target_order)
    value = target_order  #value.tolist()
    # print("target_order :", value)
    jsonEntry = {'ids' : value}
    # data.get('PGeo').update(jsonEntry)
    data.update(jsonEntry)

    # Reorder Alpha according to the shuffled indices.
    # print("DEBUG :", vtxAttrAlphaArrays)
    vtxAttrAlphaArrays = (reorder ((concatenate(vtxAttrAlphaArrays)), source_order, target_order)).tolist()
    jsonEntry = {'as' : vtxAttrAlphaArrays}    
    data.update(jsonEntry)

    # print("pre_vtxAttrCdTuples :", vtxAttrCdTuples)
    # Reorder colors according to the shuffled indices.
    # print("vtxAttrCdTuples :", vtxAttrCdTuples)
    vtxAttrCdTuples = (reorder ((array(vtxAttrCdTuples)), source_order, target_order)).tolist()
    jsonEntry = {'cs' : vtxAttrCdTuples}
    data.update(jsonEntry)

    vtxAttrNTuples = (reorder ((array(vtxAttrNTuples)), source_order, target_order)).tolist()
    jsonEntry = {'ns' : vtxAttrNTuples}
    data.update(jsonEntry)

    # Fix `/` in a file path
    vtxAttrMatDictVals = fixPaths(vtxAttrMatDictVals)
    
    jsonEntry = {'mats' : vtxAttrMatDictVals}
    data.update(jsonEntry)

    vtxAttrUVTuples = (reorder ((array(vtxAttrUVTuples)), source_order, target_order)).tolist()
    jsonEntry = {'uvws' : vtxAttrUVTuples}
    data.update(jsonEntry)

    jsonEntry = {'ps' : ptAttrPTuples}
    data.update(jsonEntry)

    # vtxAttrAlphaArrays = (reorder ((concatenate(vtxAttrAlphaArrays)), source_order, target_order)).tolist()
    jsonEntry = {'m' : concat(glAttrM)}
    data.update(jsonEntry)

    jsonEntry = {'v' : glAttrV}
    data.update(jsonEntry)

    jsonEntry = {'xform' : glAttrXform}
    data.update(jsonEntry)

    return data


def Main(fileIn = "models/cornel_box.geo", fileOut = "models/cornel_box.pgeo", skip=False):
    # print ("fileIn: {}, fileOut: {}, skip: {}".format (fileIn, fileOut, skip))
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S")
    print("Before Parsing Time: ", current_time)

    print ("Parsing data")
    data = parseJSON(readJSON(fileIn))
    # print(data)
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S")
    print("After Parsing Time      : ", current_time)

    # Write the data into a json fileIn
    with open(fileOut, 'w') as outfile:
        json.dump(data, outfile)

if __name__ == "__main__":
    descr = \
    "This application converts Houdini geo format to a pgeo, intermediary format.\n\
    It's using IPC to talk to a haskell process to do the value shuffling.\n\
    PGeo is a homeomorphic json geo container, suitable for standard haskell.\n\
    example: $ python geoParser.py inputFile.geo outputFile.pgeo\n"
    
    parser = argparse.ArgumentParser(description = descr)
    parser.add_argument("pathIn", type=str,
                        help = "a path to a model file, e.g. `python ./geoParser.py ./models/cornel_box.geo ... `")
    parser.add_argument("pathOut", type=str,
                        help = "a path to the save a model to a file, e.g. `python ./geoParser.py ... ./models/cornel_box.pgeo")
    parser.add_argument("--skip", help = "skip geometry indexing",
                        action="store_true")
    args = parser.parse_args()

    Main (args.pathIn, args.pathOut, args.skip)
    # if args.pathIn and args.pathOut and args.skip:
    #     # print("args.skip: {}".format(args.skip))
    #     Main (args.pathIn, args.pathOut, args.skip)
    # elif args.pathIn and args.pathOut:
    #     print (" pathIn: {}\n pathOut: {}".format (args.pathIn, args.pathOut))
    #     Main (args.pathIn, args.pathOut, args.skip)
    # else:
    #     print("Error")

    
#     print("This application converts Houdini geo format to a pgeo, intermediary format.\n\
# It's using IPC to talk to a haskell process to do the value shuffling, because I know no better :P.\n\
# PGeo is homeomorphic json geo container, suitable for standard haskell.\n\
# Usage: $ python geoParser.py inputFile.geo outputFile.pgeo\n")
    
#     if len(sys.argv) <= 1:
#         print("Parsing default ./models/cornel_box.geo")
#         Main()
#     else:
#         print("Parsing %s" % sys.argv[1])
#         Main(sys.argv[1], sys.argv[2])

# for i in list(set(concat(foo))):
# ...  list(filter(lambda x: x == i, concat(foo)))
# ... 
# [0, 0, 0]
# [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
