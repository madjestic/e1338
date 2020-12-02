{-# LANGUAGE TemplateHaskell #-}

module Project 
  ( Project  (..)
  , ProjectCamera (..)
  , pTransform
  , pApt
  , pFoc
  , Model    (..)
  , name
  , resx
  , resy
  , models
  , objects
  , background
  , PreObject (..)
  , pname
  , modelIDXs
  , solvers
  , solverAttrs
  , fonts
  , cameras
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

data PreObject
  =  PreObject
     {
       _pname       :: String
     , _modelIDXs   :: [Int]
     , _solvers     :: [String]
     , _solverAttrs :: [[Double]]
     } deriving Show

$(makeLenses ''PreObject)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PreObject

data ProjectCamera
  =  ProjectCamera
     {
       _pApt       :: Double
     , _pFoc       :: Double
     , _pTransform :: [Float]
     } deriving Show
$(makeLenses ''ProjectCamera)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ProjectCamera

data Project
  =  Project
     {
       _name    :: String
     , _resx    :: Int
     , _resy    :: Int
     , _models  :: [Model]
     , _objects :: [PreObject]
     -- TODO: 
     --, _foreground :: [PreObject] 
     , _background :: [PreObject]
     , _fonts   :: [Model]
     , _cameras :: [ProjectCamera]
     } deriving Show
$(makeLenses ''Project)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Project

emptyProject :: Project
emptyProject =
  Project "foobar" (-1) (-1) [] [] [] [] []

defaultProject :: Project
defaultProject =
  Project
  "Earth Test"
  800
  600
  [(Model   "models/earth.bgeo")]
  [(PreObject
    "Earth"
    [0]
    ["spin"]
    [[0,0,0,0,0,1000]]
   )]
  []
  [(Model   "models/fnt_0.bgeo")
  ,(Model   "models/fnt_1.bgeo")
  ,(Model   "models/fnt_2.bgeo")
  ,(Model   "models/fnt_3.bgeo")
  ,(Model   "models/fnt_4.bgeo")
  ,(Model   "models/fnt_5.bgeo")
  ,(Model   "models/fnt_6.bgeo")
  ,(Model   "models/fnt_7.bgeo")
  ,(Model   "models/fnt_8.bgeo")
  ,(Model   "models/fnt_9.bgeo")
  ]
  [(ProjectCamera
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-1,
     0, 0, 0, 1])]

writeProject :: Project -> FilePath -> IO ()
writeProject prj fileOut =
  B.writeFile fileOut $ encodePretty' config prj
  where
    config = defConfig { confCompare = comp }

comp :: Text -> Text -> Ordering
comp = keyOrder . (fmap pack) $ ["name", "resx", "resy", "models", "objects", "background", "pname", "modelIDXs", "solvers", "solverAttrs", "fonts", "cameras", "pApt", "pFoc", "pTransform"]

parse :: FilePath -> IO Project
parse filePath =
  do
    d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String Project)
    let name'     = (_name     . fromEitherDecode) d
        resx'     = (_resx     . fromEitherDecode) d
        resy'     = (_resy     . fromEitherDecode) d
        models'   = (_models   . fromEitherDecode) d
        objects'  = (_objects  . fromEitherDecode) d
        background'  = (_background . fromEitherDecode) d
        fonts'    = (_fonts    . fromEitherDecode) d
        cameras'  = (_cameras  . fromEitherDecode) d
    return $ Project name' resx' resy' models' objects' background' fonts' cameras'
      where
        fromEitherDecode = fromMaybe emptyProject . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt
