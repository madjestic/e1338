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
  , Material.read
  , write
  , textures
  ) where  

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH
import Data.Maybe                (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Text    hiding (drop)

import Texture as T hiding (name, _name)

data Material
  =  Material
     {
       _name       :: String
     , _vertShader :: FilePath   -- path to vertex shader program
     , _fragShader :: FilePath   -- path to fragment shader program
     , _textures   :: [Texture] -- paths to texture bindings
     } deriving Show
$(makeLenses ''Material)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Material

defaultMat :: Material
defaultMat
  = Material
    "default"
    "shader.vert"
    "shader.frag"
    [defaultTexture]

read :: FilePath -> IO Material
read jsonFile =
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
            Right pt -> Just pt
            _ -> Nothing

write :: Material -> FilePath -> IO ()
write mat fileOut =
  do
    B.writeFile fileOut $ encodePretty' config  mat
    where
      config = defConfig { confCompare = comp }

comp :: Text -> Text -> Ordering
comp = keyOrder . fmap pack $ ["name", "fragShader", "vertShader", "textures"]
