
{-
The o'reilly book: 'Haskell Data Analysis Cookbook' chap 7 sec 'create data structure for playing card' covers this module.
-}
module Numeric_Probability_Example_Collection() where

import Numeric.Probability.Example.Collection
import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution((??))


currCard :: Card 
currCard = (Plain 3,Club)

isCurrCardAFaceCard = isFace currCard
isCurrCardAPlainCard = isPlain currCard

-- from: Haskell Data Analysis Cookbook
-- always gives same answer, so not using Random, but is a straight probaility calculation
--gives answer as percentage
--If I make them the same card, get 0%, so is using sampling without replacement.
chancesOfGetting2Cards = print $ 
         Dist.just [(Plain 3, Heart), (Plain 3, Club)] ?? select 2 deck

--returns 0%, as it doing without replacement, and so the card is removed, and can't be found again.
chancesOfGettingSameCardTwice = print $ 
         Dist.just [(Plain 3, Heart), (Plain 3, Heart)] ?? select 2 deck

--answer: 1.92... which shows that it is a percentage, as I calc'd it manually.
chancesOfGetting1 = print $ 
         Dist.just [(Plain 3, Heart)] ?? select 1 deck