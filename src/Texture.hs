{-# LANGUAGE TemplateHaskell #-}

module Texture 
  ( Texture (..)
  , path
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH


data Texture
  =  Texture
     {
       _path :: String
     } deriving Show

$(makeLenses ''Texture)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Texture
