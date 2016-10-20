module RPois
    (rPois
    ) where

import System.Random (randomRIO) -- temporary. need to replace this with poisson sampling.


-- https://en.wikipedia.org/wiki/Poisson_distribution#Generating_Poisson-distributed_random_variables
rPois' :: Double -> Double -> Int -> Double -> Double -> Int
rPois' l u x p s = if u < s then x else rPois' l u x' p' s'
  where x' = x+1
        p' = p*l/(fromIntegral x)
        s' = s+p

rPois :: Double -> IO Int
rPois l = do
  u <- (randomRIO (0,1) :: IO Double)
  return $ rPois' l u 0 (exp $ -l) (exp $ -l)
