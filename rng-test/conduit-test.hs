-- {-# LANGUAGE ExtendedDefaultRules #-}
import Conduit -- conduit-combinators
import Data.Conduit.List (isolate)


-- inverse transform using sequential search
qPois' :: Double -> Double -> Int -> Double -> Double -> Int
qPois' l u x p s = if u < s then x else qPois' l u x' p' s'
  where x' = x+1
        p' = p*l/(fromIntegral x')
        s' = s+p'

qPois :: Double -> Double -> Int
qPois l u = qPois' l u 0 (exp $ -l) (exp $ -l)



source :: ConduitM () Int IO ()
source = sourceRandomN 5 =$= mapC (qPois 1)

main :: IO ()
main = do
  runConduit $ source =$= mapM_C (liftIO . print)
