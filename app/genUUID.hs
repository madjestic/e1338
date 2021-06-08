-- | This program regenerates, or "fixes", default and/or duplicate UUIDs in a
-- | project file
-- | e.g. `$ cabal run exe:gennUUID.hs ./projects/foobar
-- | > ./projects/foobar
-- | duplicate or default (nil) UUIDs inside ./projects/foobar are replaced with a
-- | fixed versions.

module Main where

import Control.Applicative ( liftA2 )
import Control.Lens        ( traverse
                           , traverseOf
                           , toListOf
                           , view
                           , set)
import Data.Aeson
import Data.UUID
import Data.UUID.V4
import Graphics.Rendering.OpenGL as GL ( GLfloat
                                       , GLuint )
import Data.Locator
import Data.List (intercalate)
import System.Directory
import System.Environment
import System.Exit
import System.IO.Unsafe
import Unsafe.Coerce

import Project as P
import Material as M
import Texture  as T
import Utils                (encodeStringUUID, (<$.>), (<*.>))

import Debug.Trace as DT

fixMaterialUUIDs :: FilePath -> IO ()
fixMaterialUUIDs path = M.read path >>= genUUID >>= flip M.write ".uuid"
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
fixProjectUUIDs path = do
  P.read path >>= genUUID  >>= flip P.write ".uuid"
  P.read path >>= genUUID' >>= flip P.write ".uuid"
  where
    genUUID = traverseOf (objects . traverse . P.uuid) (const nextRandom)
    genUUID'= traverseOf (background . traverse . P.uuid) (const nextRandom)

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
  copyFile ".uuid" filePath

parseArgs :: [String] -> IO String
parseArgs ["-h"] = help    >> exit
parseArgs ["-v"] = version >> exit
parseArgs []     = getContents
parseArgs xs     = putStrLn ("(re)Generating UUIDs for  project file: " ++ show xs) >> return (intercalate " " xs)

help    = putStrLn "Usage: genUUID [-- -vhmp] [file ..]"
version = putStrLn "genUUID 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
