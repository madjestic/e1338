module Main where

import System.Environment
import System.Exit

import Project

-- import Debug.Trace as DT

-- | This script generates a project file
-- |  example:
-- |  $ cabal run genProject Test
-- | TODO: should generate a project file, based on CLI ToArgs
-- | e.g.: `$ cabal run genProject Foo 800 600 "models/model.bgeo" "textures/texture.jpg" 0 0 0`

main :: IO ()
main = getArgs >>= parseArgs >>= Project.write defaultProject

parseArgs :: [[Char]] -> IO String
parseArgs ["-h"] = help    >> exit
parseArgs ["-v"] = version >> exit
parseArgs []     = getContents
parseArgs fs     = putStrLn ("Generating project file: " ++ show (head fs)) >> return (concat fs) 

help :: IO ()
help    = putStrLn "Usage: genProject [-- -vh] [file ..]"

version :: IO ()
version = putStrLn "genProject 0.1"

exit :: IO String
exit    = exitWith ExitSuccess

die :: IO String
die     = exitWith (ExitFailure 1)
