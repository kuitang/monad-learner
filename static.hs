{-# LANGUAGE DataKinds #-}

import GHC.TypeLits
import Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra.HMatrix as LA

a = row (vec4 1 2 3 4)
u = vec4 10 20 30 40
v = vec2 5 0 & 0 & 3 & 7

