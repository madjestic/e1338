module Main where

import System.Directory
import Unsafe.Coerce
import System.Environment        (getArgs)
import Data.List.Split

import Material

-- import Debug.Trace as DT

-- | This script generates a file structure for a material
--   example:
--   $ cabal run genMaterial -- mat/foobar -- that's formatted i.e. the error message from e1337 in case a material is missing
--   > ./mat/foobar ...

main :: IO ()
main =
  do
    args <- getArgs
    print $ "args: " ++ show args
    let
      cmdArgs    = splitOn "/" (unsafeCoerce (args!!0) :: String)
    putStrLn $ "cmdArgs: " ++ show cmdArgs
    let
      dirName    = cmdArgs!!0
      matName    = cmdArgs!!1
    putStrLn $ "dirName: " ++ show dirName
    putStrLn $ "matName: " ++ show  matName

    -- | create a shader dir in ./mat/mymatdir
    putStrLn "Generating dirs..."
    createDirectoryIfMissing True ("./mat/" ++ matName  ++ "/src")

    -- | write  shader file ./mat/mymatdir/mymat
    putStrLn "Generating Materials..."    
    let 
      matSubDirName  = ("./mat/" ++ matName ++ "/src")
      vertShaderPath = ( matSubDirName ++ "/shader.vert")
      fragShaderPath = ( matSubDirName ++ "/shader.frag")
      --mat            = Material matName vertShaderPath fragShaderPath [] -- TODO: add a default checkerboard texture with a default UUID -- TODO: add a genTexture and a texture file in the spirit of genMaterial and a Material object e.g.: "./textures/chckerboard" - {"name", "textures", "uuid"}...
      mat =
        defaultMat
        { _name       = matName
        , _vertShader = vertShaderPath
        , _fragShader = fragShaderPath }
      in
        do write mat ("./mat/" ++ matName ++ "/" ++ matName)
           copyFile "./mat/default/src/shader.vert" vertShaderPath
           copyFile "./mat/default/src/shader.frag" fragShaderPath
    
    putStrLn "OK"      
