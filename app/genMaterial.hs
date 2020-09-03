module Main where

import Data.Aeson
import System.Directory
import Unsafe.Coerce
import System.Environment        (getArgs)
import Data.Char (toUpper)
import Data.List.Split

import Material

import Debug.Trace as DT

-- | This script generates a file structure for a material
--   example:
--   $ cabal run genMaterial mat/foobar -- that's formatted i.a.w. the error message from e1337 in case a material is missing
--   > ./mat/foobar ...

main :: IO ()
main =
  do
    args <- getArgs
    print  args
    let
      cmdArgs    = splitOn "/" (unsafeCoerce (args!!0) :: String)
    putStrLn $ "cmdArgs: " ++ show cmdArgs
    let
      dirName    = cmdArgs!!0
      matName    = cmdArgs!!1
    putStrLn $ "dirName: " ++ show dirName
    putStrLn $ "matName: " ++ show  matName
    let
      capitalize = (\x -> (toUpper . head $ x):[] ++ (tail x))

    -- | create a shader dir in ./mat/mymatdir
    putStrLn "Generating dirs..."
    createDirectoryIfMissing True ("./mat/" ++ matName  ++ "/src")

    -- | write  shader file ./mat/mymatdir/mymat
    putStrLn "Generating Materials..."    
    let 
      matSubDirName  = ("./mat/" ++ matName ++ "/src")
      vertShaderPath = ( matSubDirName ++ "/shader.vert")
      fragShaderPath = ( matSubDirName ++ "/shader.frag")
      mat            = Material matName vertShaderPath fragShaderPath []
      in
        do writeMaterial mat ("./mat/" ++ matName ++ "/" ++ matName)
           copyFile "./mat/default/src/shader.vert" vertShaderPath
           copyFile "./mat/default/src/shader.frag" fragShaderPath
    
    putStrLn "OK"      
