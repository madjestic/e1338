-- | This program regenerates, or "fixes", default and/or duplicate UUIDs in a
-- | project file
-- | e.g. `$ cabal run exe:gennUUID.hs ./projects/foobar
-- | > ./projects/foobar
-- | duplicate or default (nil) UUIDs inside ./projects/foobar are replaced with a
-- | fixed versions.

module Main where

import Control.Applicative (liftA2)
import Control.Monad       ((>=>))
import Control.Lens        ( view
                           , over
                           , traverse
                           , traverseOf
                           , set)
import Data.Aeson
import Data.UUID
import Data.UUID.V4
import System.Directory
import System.Environment
import System.Exit
import System.IO.Unsafe
import Unsafe.Coerce

import Project as P

import Debug.Trace as DT

fixUUIDs :: String -> IO ()
fixUUIDs path = (P.parse path) >>= (\x -> traverseOf (objects . traverse . objID) (const nextRandom) x) >>= writeProject' "./projects/.temp"

main :: IO ()
main = do
  args     <- getArgs
  filePath <- parseArgs args
  fixUUIDs filePath
  copyFile "./projects/.temp" filePath


parseArgs :: [String] -> IO String
parseArgs ["-h"] = help    >> exit
parseArgs ["-v"] = version >> exit
parseArgs []     = getContents
parseArgs fs     = putStrLn ("(re)Generating UUIDs for  project file: " ++ show (head fs)) >> return (concat fs)

help    = putStrLn "Usage: genUUID [-- -vh] [file ..]"
version = putStrLn "genUUID 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
