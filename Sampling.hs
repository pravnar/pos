module Sampling where

-- | Importing internal modules written for this project        
import Types
import Bayes

categorical = undefined

sampleFrom = undefined

{-

instead of looking up i in storeGamma we might have to look
up (i-1), for the prev word, and then only do joint on the 
given tag from (Trans tag), and /then/ do the things with
currGamm etc that storeGamma does

-}             
