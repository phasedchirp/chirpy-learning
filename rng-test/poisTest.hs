import System.Environment
import System.Random (getStdGen,RandomGen(..),randomRs)


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


main = do
    (l:x:_) <- getArgs
    gen <- getStdGen
    let n = read x :: Int
        rate = read l :: Double
        ps = rPois gen rate n
    mapM_ putStrLn $ map show ps
