{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Material
  ( Material (..)
  , name
  , defaultMat
  , readMaterial
  , writeMaterial
  , textures
  ) where  

import Control.Monad         (mzero)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH
import Data.Maybe            (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Control.Lens hiding ((.=))
import Data.Text    hiding (drop)

data Material
  =  Material
     {
       _name       :: String
     , _vertShader :: FilePath   -- path to vertex shader program
     , _fragShader :: FilePath   -- path to fragment shader program
     , _textures   :: [FilePath] -- paths to texture bindings
     } deriving Show

$(makeLenses ''Material)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Material

defaultMat
  = Material
    "default"
    "shader.vert"
    "shader.frag"
    []

readMaterial :: FilePath -> IO Material
readMaterial jsonFile =
  do
    -- print $ "jsonFile :" ++ jsonFile
    d <- (eitherDecode <$> B.readFile jsonFile) :: IO (Either String Material)
    print $ "Loading Material :"
      ++ case d of
           Right m -> view name m
           _ -> "error"

    let name'       = (_name       . fromEitherDecode) d
        vertShader' = (_vertShader . fromEitherDecode) d
        fragShader' = (_fragShader . fromEitherDecode) d
        textures'   = (_textures   . fromEitherDecode) d
    return $ Material name' vertShader' fragShader' textures'
      where
        fromEitherDecode = fromMaybe (Material "" "" "" []) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt

writeMaterial :: Material -> FilePath -> IO ()
writeMaterial mat fileOut =
  do
    B.writeFile fileOut $ encodePretty' config  mat
    where
      config = defConfig { confCompare = comp }

comp :: Text -> Text -> Ordering
comp = keyOrder . (fmap pack) $ ["name", "fragShader", "vertShader", "textures"]
      
