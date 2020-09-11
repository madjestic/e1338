{-# LANGUAGE TemplateHaskell #-}

module Project 
  ( Project  (..)
  , Model    (..)
  , name
  , resx
  , resy
  , models
  , textures
  , camera
  , parse
  , writeProject
  , defaultProject
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy as B hiding (drop, pack)
import Data.Maybe                (fromMaybe)
import Data.Text                 hiding (drop)
--import Linear.Matrix

import Texture
import Model

data Project
  =  Project
     {
       _name     :: String
     , _resx     :: Int
     , _resy     :: Int
     , _models   :: [Model]
     , _fonts    :: [Model]
     , _textures :: [Texture]
     , _camera   :: [Float]
     } deriving Show

$(makeLenses ''Project)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Project

emptyProject :: Project
emptyProject =
  Project "foobar" (-1) (-1) [] [] [] []

defaultProject :: Project
defaultProject =
  Project
  "Default"
  800
  600
  [(Model   "models/box.bgeo")]
  []
  [(Texture "textures/checkerboard.png")]
  [1, 0, 0, 0,
   0, 1, 0, 0,
   0, 0, 1,-1,
   0, 0, 0, 1]

writeProject :: Project -> FilePath -> IO ()
writeProject prj fileOut =
  B.writeFile fileOut $ encodePretty' config prj
  where
    config = defConfig { confCompare = comp }

comp :: Text -> Text -> Ordering
comp = keyOrder . (fmap pack) $ ["name", "resx", "resy", "models", "fonts", "textures", "camera"]

parse :: FilePath -> IO Project
parse filePath =
  do
    d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String Project)
    let name'     = (_name     . fromEitherDecode) d
        resx'     = (_resx     . fromEitherDecode) d
        resy'     = (_resy     . fromEitherDecode) d
        models'   = (_models   . fromEitherDecode) d
        fonts'    = (_fonts    . fromEitherDecode) d
        textures' = (_textures . fromEitherDecode) d
        camera'   = (_camera   . fromEitherDecode) d
    return $ Project name' resx' resy' models' fonts' textures' camera'
      where
        fromEitherDecode = fromMaybe emptyProject . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt
