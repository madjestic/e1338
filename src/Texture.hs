{-# LANGUAGE TemplateHaskell #-}

module Texture 
  ( Texture (..)
  , name
  , path
  , uuid
  , defaultTexture
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH
import Data.UUID
import Data.UUID.V4
import Data.Text    hiding (drop)

import Utils (encodeStringUUID)

data Texture
  =  Texture
     {
       _name
     , _path :: FilePath -- TODO: replace with Maybe FilePath or Either (FilePath or Generated, maybe a formula?)
     , _uuid :: UUID
     } deriving Show
$(makeLenses ''Texture)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Texture

instance Eq Texture where
  t0 == t1 = view uuid t0 == view uuid t1

instance Ord Texture where
  compare t0 t1  = compare (view uuid t0) (view uuid t1)

defaultTexture
  = Texture
    "checkerboard"
    "./textures/checkerboard.png"
    (encodeStringUUID "./textures/checkerboard.png")

comp :: Text -> Text -> Ordering
comp = keyOrder . fmap pack $ ["name", "path", "uuid"]
