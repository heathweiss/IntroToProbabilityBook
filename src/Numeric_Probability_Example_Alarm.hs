module Numeric_Probability_Example_Alarm() where

import Numeric.Probability.Example.Alarm
import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Percentage(showPfix)

import Data.Ratio(Rational)

import qualified Data.List as List

import Helpers(truncate2, truncate4)

pmjAsPercentage = realToFrac pmj * 100 

burglaryAsPercentage :: (Burglary, Rational) -> (Burglary,Double)
burglaryAsPercentage (b,ratio) = (b, truncate2 $ realToFrac ratio * 100)
--burglaryAsPercentage B = (b, truncate2 $ realToFrac ratio * 100)


extract_ = Dist.extract bJoint


bJointAsPercentage = 
  zipWith (\a b -> (a,b)) extract_ runExtractProbability 
  where
  runExtractProbability = 
    map  
      (\n -> showPfix 4 $ realToFrac n )  --format each probalility
      (List.map snd $ Dist.decons bJoint :: [Rational]) --extract probaility from bJoint

prettyPrintBJointAsPercentage = mapM_ print bJointAsPercentage