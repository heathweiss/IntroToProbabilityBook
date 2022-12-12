
module Chap1() where

import Numeric.Sampling

import Data.Maybe(catMaybes)

checkPosition7 :: Maybe [Int] -> Maybe Int 
checkPosition7 (Just(_:_:_:_:_:_:seven:xs)) =
    if seven == 7
      then Just 7
      else Nothing 
checkPosition7 _ = Nothing

samplePos7 :: Int -> [Maybe Int] -> IO [Int]
samplePos7 counter workingList = do
  if counter > 0 
    then do
      int <- sampleIO 7 [1..52] :: IO (Maybe [Int])
      samplePos7 (counter - 1) (checkPosition7 int: workingList)
    else
      return $ catMaybes workingList

runSamplePosition7 = do
  ints <- samplePos7  10000 []
  pure $ length ints


resamplePos7 :: Int -> [Maybe Int] -> IO [Int]
resamplePos7 counter workingList = do
  if counter > 0 
    then do
      int <- resampleIO 7 [1..52] :: IO [Int]
      resamplePos7 (counter - 1) (checkPosition7 (Just int): workingList)
    else
      return $ catMaybes workingList

runResamplePos7 = do
  ints <- resamplePos7 10000 []
  pure $ length ints


------------------------------------------------------------------------------------------------------------------------
--do the mess around

runSum = sum [3,1,4,5,9]

sampeText = sampleIO 5 ["a","b","c","d", "e", "f"]
resample_ = resampleIO 5 [1..10]
resampleText = resampleIO 5 ["a","b","c","d", "e", "f"]
