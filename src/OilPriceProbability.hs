--{-# LANGUAGE PartialTypeSignatures #-}
module OilPriceProbability() where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Percentage(showPfix)
import Numeric.Probability.Distribution ((??), (?=<<), (>>=?))


type Probability = Rational
type Dist a = Dist.T Probability a
type PBool  = Dist.T Probability Bool


flp :: Probability -> PBool
flp p = Dist.choose p True False

-- | prior burglary 1%
iranIsBoycotted :: Bool -> Bool -> PBool
iranIsBoycotted outOfOil tightOilMarket = 
  case (outOfOil,tightOilMarket) of
    (True,True)   -> flp 0.1
    (True,False)  -> flp 0.0
    (False,False) -> flp 0.2
    (False,True)  -> flp 0.3
  
{-
iranIsBoycotted :: Bool -> PBool
iranIsBoycotted outOfOil = 
  if outOfOil 
    then flp 0
    else flp 0.2
-}

iranIsOutOfOil :: PBool
iranIsOutOfOil = flp 0

isTightOilMarket :: PBool
isTightOilMarket = flp 3.0

-- conditional Iran having uprising affect price of oil
iranIsUprising :: Bool -> PBool
iranIsUprising isBoycotted = 
  case isBoycotted of
    True  -> flp 0.2
    False -> flp 0.8


--data IranianOilPriceChange = IranianOilPriceChange {outOfOil :: Bool, boycotted :: Bool, uprising :: Bool} 
data IranianOilPriceChange = IranianOilPriceChange {outOfOil :: Bool, boycotted :: Bool, uprising :: Bool, tightOilMarket :: Bool} 

currentStateOfIran :: Dist IranianOilPriceChange
currentStateOfIran = do 
  iranIsOutOfOil_  <- iranIsOutOfOil 
  isTightOilMarket_ <- isTightOilMarket
  iranIsBoycotted_ <- iranIsBoycotted iranIsOutOfOil_ isTightOilMarket_
  iranIsUprising_  <- iranIsUprising iranIsBoycotted_
  return $ IranianOilPriceChange iranIsOutOfOil_ iranIsBoycotted_  iranIsUprising_ isTightOilMarket_

boycottedThenUprising :: Probability
boycottedThenUprising = boycotted ?? uprising ?=<< currentStateOfIran
runBboycottedThenUprising = showPfix 4 boycottedThenUprising

--(?=<<) :: Fractional prob => (a -> Bool) -> T prob a -> T prob a 
--(>>=?) :: Fractional prob => T prob a -> (a -> Bool) -> T prob a 
--(??) :: Num prob => Event a -> T prob a -> prob 
--type Event a = a -> Bool
willNotAffectOilPriceIfOutOfOil :: Probability
willNotAffectOilPriceIfOutOfOil =   (outOfOil  ?? ((boycotted ?=<< currentStateOfIran) >>=? uprising)) 
runWwillNotAffectOilPriceIfOutOfOil = print . showPfix 4 $ realToFrac willNotAffectOilPriceIfOutOfOil

isBoycotedAndHasUprising :: Dist.T Probability IranianOilPriceChange
isBoycotedAndHasUprising =  isBoycottedT >>=? uprising 

hasUprisingInTightOilMarket :: Probability 
hasUprisingInTightOilMarket = uprising ?? tightOilMarket ?=<< currentStateOfIran

hasUprising :: Dist.T Probability IranianOilPriceChange
hasUprising = uprising ?=<< currentStateOfIran
-------------------------------------------------------------------------------------------------------------------
isBoycotedAndHasUprisingProb :: Probability
--this works
--isBoycotedAndHasUprisingProb = boycotted ?? hasUprising -- ??  >>=? currentStateOfIran
--and so does this
isBoycotedAndHasUprisingProb = boycotted ?? uprising ?=<< currentStateOfIran
prettyIisBoycotedAndHasUprisingProb = showPfix 4 isBoycotedAndHasUprisingProb

isBoycottedT :: Dist.T Probability IranianOilPriceChange
isBoycottedT =  boycotted ?=<< currentStateOfIran 

uprisingThenBoycotted :: Probability
uprisingThenBoycotted =  uprising ?? boycotted ?=<< currentStateOfIran

isOutOfOilBoycottedAndUprising :: Probability
isOutOfOilBoycottedAndUprising = outOfOil  ?? isBoycotedAndHasUprising