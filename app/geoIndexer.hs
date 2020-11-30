{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Text.Lazy.IO as I    hiding (putStrLn)
import Data.ByteString   as BS   (writeFile)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Store                (encode)
       
import Unsafe.Coerce
import System.Environment        (getArgs)

import Options.Applicative
import Data.Semigroup ((<>))

import PGeo
import VGeo

--import Debug.Trace as DT

data GeoArgs =
  GeoArgs
  {
    fileIn  :: FilePath
  , fileOut :: FilePath
  , skip    :: Bool
  } deriving Show

geoArgs :: Parser GeoArgs
geoArgs = GeoArgs
       <$> strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> value "./models/model.pgeo"
         <> help "Read source model file")
       <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value "./models/model.bgeo"
         <> help "Write output model file")
       <*> switch
          ( long "skip"
         <> short 's'
         <> help "Whether to perform the indexing" )

writeVGeo :: FilePath -> VGeo -> IO ()
writeVGeo fileOut vgeo =
  do
    I.writeFile "../models/debug.vgeo"
      (encodeToLazyText ( is vgeo
                        , st vgeo
                        , vs vgeo
                        , ms vgeo
                        , xf vgeo ))

writeBGeo :: FilePath -> VGeo -> IO ()
writeBGeo fileOut vgeo =
  do
    BS.writeFile fileOut $ encode $ ( is vgeo, st vgeo, vs vgeo, ms vgeo, xf vgeo )

main :: IO ()
main = do
  let
    opts = info (geoArgs <**> helper)
        ( fullDesc
       <> progDesc "(optionaly) reduce repetitions in a pgeo and save as a bgeo"
       <> header "(optionally) index (pgeo List -> bgeo Set)" )
  args <- execParser opts
  putStrLn $ "args :" ++ show args

  -- args <- getArgs -- TODO: add a skip switch here
  -- let fileIn  =  (unsafeCoerce (args!!0) :: FilePath)
  --     fileOut =  (unsafeCoerce (args!!1) :: FilePath)
  --     index   =  (unsafeCoerce (args!!2) :: FilePath)
  
  pgeo <- readPGeo (fileIn args)
  putStrLn "running indexer..."
  --let vgeo = fromPGeo pgeo
  let vgeo = case (skip args) of
        False -> fromPGeo  pgeo
        True  -> fromPGeo' pgeo
  -- _ <- DT.trace ("geoIndexer.vgeo :" ++ show vgeo) $ return ()
  putStrLn "OK"
  writeBGeo (fileOut args) vgeo
  -- writeVGeo fileOut vgeo
