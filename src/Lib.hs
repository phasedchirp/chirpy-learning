module Lib
    ( step
    -- , covStep
    , cumulativeStats
    , cumulativeStatsWeighted
    -- , cumulativeStats'
    , summarizeMean
    -- , summarizeCov
    ) where

import RPois
import System.Random (getStdGen)
import Control.Monad (replicateM,join)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource (ResourceT)


-- Haskell implementation of Welford's algorithm for online estimation of mean and variance.
step :: (Floating a) => (a,a,a) -> a -> (a,a,a)
step (n,m,m2) x = (nNew,mNew,m2New)
                  where delta = x-m
                        nNew = n+1
                        mNew = m+delta/nNew
                        m2New = m2+delta*(x-mNew)

-- Takes starting point for estimators, returns conduit that incrementally updates
-- as data is received. `join` needed because twitter-conduit results introduce an
-- extra layer of Maybe.
cumulativeStats :: (Floating t, Monad m) => (t, t, t) -> ConduitM (Maybe t) (t, t, t) m b
cumulativeStats s = do
  val <- await
  case (join val) of
    Just x -> do
      let updated = step s x
      yield updated
      cumulativeStats updated
    Nothing -> do
      cumulativeStats s

-- Function to print out estimates as they are updated.
summarizeMean :: ConduitM (Double, t, Double) (t, Double) (ResourceT IO) b
summarizeMean = do
  val <- await
  case val of
    Just (n,m,m2) -> do
      yield (m,sqrt (m2/n))
      summarizeMean
    Nothing -> do
      summarizeMean

--
covStep :: (Floating a) => (a,a) -> (a,a,a,a) -> (a,a,a,a)
covStep (x1,x2) (n,m1,m2,m12) = (nNew,m1New,m2New,m12New)
                                where nNew = n+1
                                      delta1 = (x1-m1)/nNew
                                      delta2 = (x2-m2)/nNew
                                      m1New = m1 + delta1
                                      m2New = m2 + delta2
                                      m12New = m12 + n*delta1*delta2 - m12/nNew



stepWeighted :: (Floating a) => (a,a,a) -> (a,Int) -> (a,a,a)
stepWeighted (n,m,m2) (x,w) = (nNew,mNew,m2New)
                      where w' = (fromIntegral w)
                            delta = x-m
                            nNew = n + w'
                            r = delta*w'/nNew
                            mNew = m + r
                            m2New = n*delta*r

cumulativeStatsWeighted :: (Floating t, Monad m) => (t, t, t) -> ConduitM (Maybe t,Int) (t, t, t) m b
cumulativeStatsWeighted s = do
  val <- await
  case val of
    Just (Just x,w) -> do
      let updated = stepWeighted s (x,w)
      yield updated
      cumulativeStatsWeighted updated
    Just _ -> do
      cumulativeStatsWeighted s
    Nothing -> do
      cumulativeStatsWeighted s
-- replicateWeighted :: Int -> ConduitM Double [(Double, Int)] (ResourceT IO) b
-- replicateWeighted n = do
--   val <- await
--   case val of Just x -> do
--                 g <- getStdGen
--                 yield $ zip (replicate n x) (rPois g 1 n)
--                 replicateWeighted n
--               Nothing -> do
--                 replicateWeighted n
-- replicateWeighted n = awaitForever $ (\x -> yield $ f x)
  -- where f x = fmap (zipWith (,) (replicate n x)) rs
        -- rs = replicate n (rPois g 1 n)

--
-- cumulativeStats' :: (Floating t, Monad m) => (t, t, t) -> ConduitM (Maybe t) (t, t, t) m b
-- cumulativeStats s = do
--   val <- await
--   case val of
--     Just (Just x) -> do
--       let updated = step s x
--       yield updated
--       cumulativeStats updated
--     Just Nothing -> do
--       cumulativeStats s
--     Nothing -> do
--       yield s
--       cumulativeStats s
--
-- --
-- summarizeCov :: ConduitM (Double, t, Double) (t, Double) (Control.Monad.Trans.Resource.Internal.ResourceT IO) b
-- summarizeMean = do
--   val <- await
--   case val of
--     Just (n,m,m2) -> do
--       yield (m,sqrt (m2/n))
--       summarize
--     Nothing -> do
--       summarize
