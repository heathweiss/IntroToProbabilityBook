module Helpers(truncate2, truncate4) where

truncate2 :: Double -> Double
truncate2 val =
  let
    t = 100 --which is 10^precision. If want to be 1/1000 it would be 10^3. 
  in
    fromIntegral (truncate(val*t))/t

truncate4 :: Double -> Double
truncate4 val =
  let
    t = 10000 --which is 10^precision. If want to be 1/1000 it would be 10^3. 
  in
    fromIntegral (truncate(val*t))/t