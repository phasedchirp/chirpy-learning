{-# LANGUAGE OverloadedStrings #-}

module Main where


import Web.Twitter.Conduit
import Web.Twitter.Conduit.Stream
import Web.Twitter.Types.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class (liftIO)
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


-- basic example pipeline. Computes a running estimate of mean and variance for tweet length
-- Could replace the initial estimate (0,0,0) with pseudo-data to act as a prior
-- Estimator defined in Lib.hs
pipeline = CL.map process =$= CL.map (fmap fst) =$= cumulativeStats (0,0,0) =$= summarizeMean

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    runResourceT $ do
      src <-stream twInfo mgr query
      src $$+- pipeline =$= CL.isolate 100 =$= CL.mapM_ (liftIO . processPrint)
