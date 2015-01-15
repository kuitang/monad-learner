{-# LANGUAGE FlexibleContexts, ConstraintKinds, DataKinds #-}
import GHC.TypeLits
import Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra as LA
import System.Environment
import Text.Printf (printf)

-- TODO: Learn about statically typed matrices.
-- TODO: Figure out how to avoid converting b/t lists and matrices.
--       (Will basically require slicing and mapping over rows/colmns).
--
--       Solution: extractRows and extractCols after the predictions line.
--       Could get rid of the toColumns entry as well and separately extract.
--       (Not really worth the time.)

-- Create design matrix for polynomial powers.
-- (or, just use Vector Double. But we want to keep generality.)
-- expand :: (Element t, Field t, Num (Vector t)) => Vector t -> Int -> Matrix t
expand :: (KnownNat n, KnownNat d) => R n -> L n d
expand x deg = fromColumns $ map (x^) [0..deg]

regress :: (Element t, Field t, Num (Vector t)) => Vector t -> Vector t -> Int -> Vector t
regress x y deg = (expand x deg) <\> y
--                                  ^ efficient pseudoinverse

evalPoly :: (Element t, Field t, Product t, Num (Vector t)) => Vector t -> Vector t -> Vector t
evalPoly coeff x = expand x (dim coeff - 1) <> coeff

-- sumVector :: (Element t, Field t) => Vector t -> t
squareDiff :: Vector Double -> Vector Double -> Vector Double
squareDiff x y = mapVector (flip (^) 2) (x - y)
sumVector :: Vector Double -> Double
sumVector x    = x <.> ones where ones = konst 1 (dim x)
sumSquareDiff x y = sumVector $ squareDiff x y

-- NB: This requires concrete types due to subroutines called:
-- readFile   :: FilePath -> IO String
-- readMatrix :: String -> Matrix Double
readData :: String -> IO [Vector Double]
readData filename = fmap (toColumns . readMatrix) $ readFile filename

maxDeg = 5
main = do
  args <- getArgs
  if null args
     then putStrLn "Usage: polyreg datafile"
     else do [x,y] <- readData "polyreg_data.txt"
             let models      = map (regress x y) [1..maxDeg]
                 predictions = map (flip evalPoly x) models
                 residuals   = map (squareDiff y) predictions
                 predsview   = fromColumns $ [x, y] ++ predictions
                 residsview  = fromColumns $ [x, y] ++ residuals
                 totalerrs   = map sumVector residuals
                 putFormat   = putStrLn . format "  " (printf "%.2f")

             putStrLn "Modeling predictions"
             putFormat predsview

             putStrLn "Modeling residuals"
             putFormat residsview

             putStrLn "Total errors"
             putFormat $ fromRows [fromList totalerrs]

