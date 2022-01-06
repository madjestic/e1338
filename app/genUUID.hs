-- | This program regenerates, or "fixes", default and/or duplicate UUIDs in a
-- | project file
-- | e.g. `$ cabal run exe:gennUUID.hs ./projects/foobar
-- | > ./projects/foobar
-- | duplicate or default (nil) UUIDs inside ./projects/foobar are replaced with a
-- | fixed versions.

module Main where

import Control.Lens        ( traverseOf
                           , toListOf
                           , set)
import Data.UUID.V4
import Data.List (intercalate)
import System.Directory
import System.Environment
import System.Exit

import Project as P
import Material as M
import Texture  as T
import Utils                (encodeStringUUID, (<$.>), (<*.>))

-- import Debug.Trace as DT

fixMaterialUUIDs :: FilePath -> IO ()
fixMaterialUUIDs path' = M.read path' >>= genUUID >>= flip M.write ".tmp.uuid"
  where
    genUUID mat = do
      let
        names = toListOf (textures . traverse . T.name) mat :: [String]
        paths = toListOf (textures . traverse . T.path) mat :: [FilePath]
        uuids = fmap encodeStringUUID paths
        txs = Texture <$.> names <*.> paths <*.> uuids
        result = set textures txs mat
      return result

fixProjectUUIDs :: FilePath -> IO ()
fixProjectUUIDs path' = do
  P.read path' >>= genUUIDfgr >>= genUUIDbgr >>= flip P.write ".tmp.uuid"
  where
    genUUIDfgr = traverseOf (objects . traverse . P.uuid) (const nextRandom)
    genUUIDbgr = traverseOf (background . traverse . P.uuid) (const nextRandom)

main :: IO ()
main = do
  args     <- getArgs
  args'    <- parseArgs args
  let
    mode     = head . words $ args'
    filePath = last . words $ args'
  case mode of
    "-m" -> fixMaterialUUIDs filePath
    "-p" -> fixProjectUUIDs  filePath
    _    -> error $ "wrong input: " ++ show mode
  copyFile ".tmp.uuid" filePath

parseArgs :: [String] -> IO String
parseArgs ["-h"] = help    >> exit
parseArgs ["-v"] = version >> exit
parseArgs []     = getContents
parseArgs xs     = putStrLn ("(re)Generating UUIDs for  project file: " ++ show xs) >> return (intercalate " " xs)

help :: IO ()
help    = putStrLn "Usage: genUUID [-- -vhmp] [file ..]"

version :: IO ()
version = putStrLn "genUUID 0.1"

exit :: IO String
exit    = exitWith ExitSuccess

die :: IO String
die     = exitWith (ExitFailure 1)
