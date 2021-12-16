{-# LANGUAGE TemplateHaskell #-}

module Project 
  ( Project  (..)
  , ProjectCamera (..)
  , pTransform
  , pApt
  , pFoc
  , pMouseS
  , pKeyboardRS
  , pKeyboardTS
  , Model    (..)
  , name
  , resx
  , resy
  , camMode
  , models
  , objects
  , background
  , PreObject (..)
  , pname
  , modelIDXs
  , uuid 
  , solvers
  , solverAttrs
  , fonts
  , cameras
  , Project.read
  , write
  , defaultProject
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy as B hiding (drop, pack)
import Data.Maybe                       (fromMaybe)
import Data.Sort                        (sortOn)                              
import Data.Text                 hiding (drop)
import Data.UUID

import Texture hiding (name, _name, uuid, _uuid)
import Model

import Debug.Trace as DT

data PreObject
  =  PreObject
     {
       _pname       :: String
     , _uuid        :: UUID
     , _modelIDXs   :: [Int]
     , _solvers     :: [String]
     , _solverAttrs :: [[Double]]
     } deriving Show

$(makeLenses ''PreObject)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PreObject

data ProjectCamera
  =  ProjectCamera
     {
       _pApt        :: Double
     , _pFoc        :: Double
     , _pTransform  :: [Float]
     , _pMouseS     :: Double -- | mouse    "sensitivity"
     , _pKeyboardRS :: Double -- | keyboard "sensitivity"
     , _pKeyboardTS :: Double
     } deriving Show
$(makeLenses ''ProjectCamera)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ProjectCamera

data Project
  =  Project
     {
       _name       :: String
     , _resx       :: Int
     , _resy       :: Int
     , _camMode    :: String
     , _models     :: [Model] -- is that used?
     , _objects    :: [PreObject]
     , _background :: [PreObject]
     , _fonts      :: [Model]
     , _cameras    :: [ProjectCamera]
     } deriving Show
$(makeLenses ''Project)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Project

emptyProject :: Project
emptyProject =
  Project "foobar" (-1) (-1) "AbsoluteLocation" [] [] [] [] []

defaultProject :: Project
defaultProject =
  Project
  "Test Project"
  800
  600
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ (PreObject
    "Box"
    nil
    [0]
    ["rotate", "translate"]
    [[0,0,0,0,0,1000]
    ,[1000,0,0]]
    )
  ]
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
     0, 0, 1,-10,
     0, 0, 0, 1])
    1.0
    1.0
    1.0
  ]

write :: Project -> FilePath -> IO ()
write prj fileOut =
  B.writeFile fileOut $ encodePretty' config prj
  where
    config = defConfig { confCompare = comp }

comp :: Text -> Text -> Ordering
comp = keyOrder . (fmap pack) $ ["name", "resx", "resy", "camMode", "models", "objects", "background", "pname", "uuid ", "modelIDXs", "solvers", "solverAttrs", "fonts", "cameras", "pApt", "pFoc", "pTransform", "pMouseS", "pKeyboardRS", "pKeyboardTS"]

read :: FilePath -> IO Project
read filePath =
  do
    d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String Project)
    let name'     = (_name     . fromEitherDecode) d
        resx'     = (_resx     . fromEitherDecode) d
        resy'     = (_resy     . fromEitherDecode) d
        camMode'  = (_camMode  . fromEitherDecode) d
        models'   = (_models   . fromEitherDecode) d
        preObjs'  = (_objects  . fromEitherDecode) d
        bgrObjs'  = (_background . fromEitherDecode) d
        fonts'    = (_fonts    . fromEitherDecode) d
        cameras'  = (_cameras  . fromEitherDecode) d
    return $
      Project
      name'
      resx'
      resy'
      camMode'
      models'
      --(sortOn (view uuid ) preObjs')
      (sortOn (view uuid ) preObjs')
      (sortOn (view uuid ) bgrObjs')
      fonts'
      cameras'
      
      where
        fromEitherDecode = fromMaybe emptyProject . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt            
