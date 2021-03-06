module RPois
    (rPois
    ) where

import System.Random (RandomGen(..),randomRs)


-- https://en.wikipedia.org/wiki/Poisson_distribution#Generating_Poisson-distributed_random_variables

-- inverse transform using sequential search
qPois' :: Double -> Double -> Int -> Double -> Double -> Int
qPois' l u x p s = if u < s then x else qPois' l u x' p' s'
  where x' = x+1
        p' = p*l/(fromIntegral x')
        s' = s+p'

qPois :: Double -> Double -> Int
qPois l u = qPois' l u 0 (exp $ -l) (exp $ -l)

-- Generate list of poisson-distributed random numbers
rPois :: (RandomGen g) => g -> Double -> Int -> [Int]
rPois g l n = map (qPois l) (take n $ randomRs (0,1) g)
