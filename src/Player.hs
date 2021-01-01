module Player where

import Object
import Camera

data Player
  =  Player
     {
       objects :: [Objects]
     , cameras :: [Camera]  -- First / 3D person, extra cams
     } deriving Show
