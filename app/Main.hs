{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random (randomRIO)
import Web.Twitter.Conduit
import Web.Twitter.Conduit.Stream
import Web.Twitter.Types.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Lens ((^.))
import Control.Monad.Trans.Resource (runResourceT)
import Lib -- estimators
import Info -- twitter API credentials

query :: APIRequest StatusesFilter StreamingAPI
query = statusesFilter [Track ["dog","fire"]]

isVowel :: Char -> Bool
isVowel x = x `elem` ("aiueo"::[Char])


-- Feature extraction (here just getting length and %consonant characters for testing)
process :: StreamingAPI -> Maybe (Double,Double)
process (SStatus status ) =
  Just (tweetLength, propVowels)
    where tweet = status ^. statusText
          tweetLength = (fromIntegral $ T.length tweet ):: Double
          propVowels = (fromIntegral $ T.length $ T.filter (not . isVowel) tweet) / (tweetLength)
process _ = Nothing


-- Print incrementally updated results
processPrint :: (Show a) => a -> IO ()
processPrint = do
  T.putStrLn . T.pack . show



-- inverse transform using sequential search
qPois' :: Double -> Double -> Int -> Double -> Double -> Int
qPois' l u x p s = if u < s then x else qPois' l u x' p' s'
  where x' = x+1
        p' = p*l/(fromIntegral x')
        s' = s+p'

qPois :: Double -> Double -> Int
qPois l u = qPois' l u 0 (exp $ -l) (exp $ -l)


-- poisSource :: Monad m => Source m Int
-- poisSource = CL.sourceList [1..] =$= CL.map (qPois 1)
--
-- poisSource' = newResumableSource poisSource



foo :: MonadIO m => Conduit StreamingAPI m (Maybe Double,Int)
foo = CL.mapM go where
  go u = do
      s <- liftIO $ (randomRIO (0,1) :: IO Double)
      let s' = qPois 1 s
      return (fmap fst $ process u,s')

-- blah = fuseBoth (CL.map process =$= CL.catMaybes =$= CL.map fst) poisSource
-- basic example pipeline. Computes a running estimate of mean and variance for tweet length
-- Could replace the initial estimate (0,0,0) with pseudo-data to act as a prior
-- Estimator defined in Lib.hs
-- pipeline = CL.map process =$= CL.map (fmap fst) =$= cumulativeStats (0,0,0) =$= summarizeMean
pipeline = foo =$= cumulativeStatsWeighted (0,0,0) =$= summarizeMean

main :: IO ()
-- main = return ()
main = do
    mgr <- newManager tlsManagerSettings
    runResourceT $ do
      src <- stream twInfo mgr query
      -- Only processes 100 tweets here.
      src $$+- pipeline =$= CL.isolate 100 =$= CL.mapM_ (liftIO . print)
