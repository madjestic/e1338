{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Text.Lazy.IO as I    hiding (putStrLn)
import Data.ByteString   as BS   (writeFile)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Store                (encode)
import Data.Time.Clock.Compat
import Data.Time.LocalTime.Compat

import Options.Applicative

import PGeo

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


formatTime' :: UTCTime -> String
formatTime' = take 8 . show . timeToTimeOfDay . utctDayTime

formatTime'' :: UTCTime -> String
formatTime'' = take 8 . drop 11 . show . (utcToLocalTime (TimeZone 60 False "AMS"))

writeVGeo :: FilePath -> VGeo -> IO ()
writeVGeo fileOut' vgeo =
  do
    --I.writeFile "../models/debug.vgeo"
    I.writeFile fileOut'
      (encodeToLazyText ( is vgeo
                        , st vgeo
                        , vs vgeo
                        , ms vgeo
                        , xf vgeo ))

writeBGeo :: FilePath -> VGeo -> IO ()
writeBGeo fileOut' vgeo =
  do
    -- print $ "writeBGeo.vgeo :" ++ show vgeo
    BS.writeFile fileOut' $ encode $ ( is vgeo, st vgeo, vs vgeo, mts vgeo, ms vgeo, vls vgeo, xf vgeo )

main :: IO ()
main = do
  let
    opts = info (geoArgs <**> helper)
        ( fullDesc
       <> progDesc "(optionaly) reduce repetitions in a pgeo and save as a bgeo"
       <> header "(optionally) index (pgeo List -> bgeo Set)" )
  args <- execParser opts
  putStrLn $ "args :" ++ show args

  pgeo <- readPGeo (fileIn args)
  -- print $ "geoIndexer.pgeo :" ++ show pgeo
  putStrLn "running indexer..."
  let vgeo =
        if skip args 
        then fromPGeo  pgeo
        else fromPGeo' pgeo
  -- _ <- DT.trace ("geoIndexer.vgeo :" ++ show vgeo) $ return ()
  currentTime' <- getCurrentTime
  putStrLn $ "Finished converting PGeo: " ++ formatTime'' currentTime'
  
  writeBGeo (fileOut args) vgeo
  currentTime'' <- getCurrentTime
  putStrLn $ "Finished writing BGeo   : " ++ formatTime'' currentTime''
  -- writeVGeo fileOut vgeo
