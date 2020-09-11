#!/bin/sh

# Font
cabal build
cabal run e1338 ./projects/font

# Box
# cabal build
# cabal run e1338 ./projects/box

# Test
# cabal build
# cabal run e1338 ./projects/box

# Earth and ISS
# planet Earth modelling and shading
# mvlink ./e1337.cabal      /home/madjestic/Projects/Haskell/e1337/e1337.cabal.earth 
# mvlink ./app/Main.hs      /home/madjestic/Projects/Haskell/e1337/app/Main.hs.test
# mvlink ./src/Rendering.hs /home/madjestic/Projects/Haskell/e1337/src/Rendering.hs.earth 
# cabal build
# cabal run e1338 ./projects/earth_iss_v02
