module Main where

import Data.Aeson
import System.Environment
import System.Exit
import Unsafe.Coerce

import Project

import Debug.Trace as DT

-- | This script generates a project file
--   example:
--   $ cabal run genProject Test
-- --   $ cabal run genProject Foo 800 600 "models/model.bgeo" "textures/texture.jpg" 0 0 0

main :: IO ()
main = getArgs >>= parse' >>= writeProject defaultProject

parse' ["-h"] = help    >> exit
parse' ["-v"] = version >> exit
parse' []     = getContents
parse' fs     = putStrLn ("Generating project file: " ++ show (fs!!0)) >> return (concat fs) 

help    = putStrLn "Usage: genProject [-- -vh] [file ..]"
version = putStrLn "genProject 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
