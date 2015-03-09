module Sampling where

-- | Importing external modules/libraries
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified System.Random.MWC as MWC
import Data.List (maximumBy)
import Data.Ord
import qualified Control.Monad as CM
import qualified Control.Monad.Trans.State as St
import Control.Monad.Trans.Class (lift)
import qualified Data.Number.LogFloat as LF    
    
-- | Importing internal modules written for this project        
import Types
import Bayes

sampling :: Tables -> Sentence -> Rand -> IO TaggedSent
sampling tables sentence rand = CM.liftM snd (eval tables memoTag)
    where memoTag = S.foldlWithIndex build initMem sentence
          initMem = return (Start, emptySent)
          build mem i word = do (pos, tSent) <- mem
                                (pos', tWord) <- tagWord word i pos
                                return (pos', extend tSent tWord)
          tagWord word i pos = do
            let (left,right) = except i sentence
            forGamma <- forPart left word i pos
            backGamma <- checkFirst (backward tables right) i
            let ratedTags = getRatedTags tables forGamma backGamma word
            drawnTag <- lift (sampleFrom (categorical ratedTags) rand)
            return (Trans drawnTag, (word, drawnTag))
          forPart left word i pos =
              case pos of
                Start -> forward tables word i
                Trans preTag -> do
                         gamma <- forward tables word (i-1)
                         let preWord = getLast left
                             preVal = M.findWithDefault 0 preTag gamma
                         return (allTags
                                 (\tag ->
                                  forAdd tables preWord tag preTag preVal 0))
                            
-- Functions for defining categorical distributions
--------------------------------------------------------------------------------    

-- | An even shorter name for PRNGs in the 'IO' monad.
type Rand = MWC.GenIO

-- | The probability density function used in both target and proposal distributions.
-- Given an input point, this method returns a probability density. 
type Density a = a -> Prob

-- | A procedure that, given a source of randomness, returns an action that 
-- produces a sample. The type itself is read as a verb, i.e, "to sample".
type Sample a = Rand -> IO a    

data Dist a = D (Density a) (Sample a)

-- Uniform -- 

-- Univariate            

-- | Univariate uniform distribution over real numbers. The parameters should 
-- not be equal. 
uniform :: (MWC.Variate a, Real a) => a -> a -> Dist a
uniform a b
    | b < a = makeUniform b a
    | a < b = makeUniform a b
    | otherwise = error "Wrong parameters for Uniform distribution"

unif1D :: Real a => a -> a -> a -> Prob
unif1D a b x
    | x < a = 0
    | x > b = 0
    | otherwise = 1 / realToFrac (b - a)

makeUniform :: (MWC.Variate a, Real a) => a -> a -> Dist a
makeUniform a b = D (unif1D a b) (MWC.uniformR (a,b))

-- Categorical --

-- | Categorical distribution over instances of the Eq typeclass.
-- The input argument is a list of category-proportion pairs. 
-- 
-- The input proportions represent relative weights and are not 
-- required to be normalized.

categorical :: (Show a, Eq a) => [(a, Prob)] -> Dist a
categorical = categoricalNormed . normalize

-- | Normalize the weights in a list of category-weight pairs.
normalize :: (Show a) => [(a, Prob)] -> [(a, Prob)]
normalize catProbs = map norm catProbs
    where norm = second (flip (/) s)
          s = foldl (+) 0 $ (snd.unzip) catProbs

-- | Assume that the weights are already normalized. This is useful
-- as an optimized version of @categorical@.
categoricalNormed :: (Show a, Eq a) => [(a,Prob)] -> Dist a
categoricalNormed catProbs =
    -- CHECK: Should fromMaybe default to "error" instead of 0?
    let dens a = sum [ p | (b,p) <- catProbs, a == b ]
    in D dens (categoricalSF catProbs)

categoricalSF :: (Show a) => [(a, Prob)] -> Sample a
categoricalSF catProbs g = do
  u <- CM.liftM LF.logFloat (sampleFrom (uniform 0 1) g)
  let (cats, probs) = unzip catProbs
      catsCDF = zip cats $ init $ scanl (+) 0 probs
  -- putStrLn $ "u = " ++ show u
  -- putStrLn $ "catsCDF = " ++ show catsCDF
  let filtered = filter ((u>=).snd) catsCDF
  return $ fst $ maximumBy (comparing snd) filtered

-- | This function can be used to call the sampling method of a distribution.
sampleFrom :: Dist a -> Sample a
sampleFrom (D _ s) = s

first  :: (a -> a') -> ((a,b) -> (a',b))
second :: (b -> b') -> ((a,b) -> (a,b'))
          
first  f = \ (a,b) -> (f a, b)
second g = \ (a,b) -> (a, g b)                     
     
