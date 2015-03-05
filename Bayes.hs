module Bayes ( bayes ) where

-- | Importing external modules/libraries
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List (foldl')
import qualified Data.Foldable as F    
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as St -- ^ State monad

-- | Importing internal modules written for this project    
import Types

type Gamm a = MemTable Tag a

-- Helper functions on Gamms
--------------------------------------------------------------------------------    
allTags :: (Tag -> a) -> Gamm a
allTags f = foldl' (\g t -> M.insert t (f t) g) M.empty tags

dummyTag :: Tag
dummyTag = PUNC
            
close :: a -> Gamm a
close a = M.singleton dummyTag a
--------------------------------------------------------------------------------

-- | A Tag -> Prob memotable
type Gamma = Gamm Prob    

-- Helper functions on Gammas
--------------------------------------------------------------------------------    
fromPT :: PT Tag -> Gamma
fromPT pt = M.mapWithKey (\k _ -> prob k pt) (counts pt)

fromStart :: Tables -> Gamma
fromStart tables = fromPT (start tables)

gammaFind :: Tag -> Gamma -> Prob
gammaFind = M.findWithDefault 0
--------------------------------------------------------------------------------

-- | An inference is a probabilistic computation that first requires a tag
-- It is a Tag -> Prob function

-- This is used to encode the various parts of our equation that require
-- the same tag, i.e., the state Si = s over which we are doing arg max

-- The Reader monad allows us to write computations that all depend on a state,
-- and then pass the *same state* to all those computations eventually
type Inference = R.Reader Tag Prob
   
-- | A Tag -> Inference memotable
type Gamma2 = Gamm Inference

-- Helper functions on Inferences
--------------------------------------------------------------------------------    
retrieve :: Gamma2 -> Inference
retrieve = M.findWithDefault (return 0) dummyTag

makeInference :: (Tag -> Prob) -> Inference
makeInference = R.reader

infer :: Inference -> Tag -> Prob
infer = R.runReader

inferProd :: Inference -> Inference -> Inference
inferProd infer1 infer2 = do
  p1 <- infer1
  p2 <- infer2
  return (p1*p2)

inferScale :: Prob -> Inference -> Inference
inferScale p1 = inferProd (return p1)

inferAdd :: Inference -> Inference -> Inference
inferAdd infer1 infer2 = do
  p1 <- infer1
  p2 <- infer2
  return (p1+p2)
--------------------------------------------------------------------------------

type MemLeft a = Mem Int Gamma a

initMemLeft :: Tables -> MemTable Int Gamma
initMemLeft tables = M.singleton 0 (fromStart tables)
    
bayes2 :: Tables -> Sentence -> MemLeft TaggedSent
bayes2 tables sentence = S.foldlWithIndex build initMem sentence
    where initMem = return emptySent
          build mem i word = do taggedSent <- mem
                                taggedWord <- tagWord word i
                                return (extend taggedSent taggedWord)
          tagWord word i = do
            gMap <- St.get
            let prevGamm = M.findWithDefault (fromStart tables) (i-1) gMap
                currGamm = newGamm tables prevGamm word
            St.put (M.insert i currGamm gMap)
            let initG2 = buildG2 tables word prevGamm
                (_, right) = except i sentence
                finalG2 = rightBuild tables initG2 right
                probTag = retrieve finalG2
                rate tag = (tag, infer probTag tag)
            return (word, bestTag (map rate tags))
{-

1 looks up 0 <--- call this old
  updates old with newGamm word1 <--- call this updatedGamm
  inserts 1 updatedGamm; put this new map
  uses old to do buildG2

2 looks up 1 <--- call this old
  updates old with newGamm word2 <--- call this updatedGamm
  inserts 2 updatedGamm; put this new map
  uses old to do buildG2

-}                           

bayes :: Tables -> Sentence -> TaggedSent
bayes tables sentence = S.mapWithIndex tagWord sentence
    where tagWord i word = 
              let (left, right) = except i sentence
                  leftGamm = leftBuild tables left
                  initG2 = buildG2 tables word leftGamm
                  finalG2 = rightBuild tables initG2 right
                  probTag = retrieve finalG2
                  rate tag = (tag, infer probTag tag)
              in (word, bestTag (map rate tags))

leftBuild :: Tables -> Sentence -> Gamma
leftBuild tables left = F.foldl' (newGamm tables) startGamm left
    where startGamm = fromPT (start tables)

newGamm :: Tables -> Gamma -> Word -> Gamma
newGamm tables oldGamm word = foldl' (update word) oldGamm tags
    where update word old tag = M.insert tag (marg tables old word tag) old

joint :: Tables -> Gamma -> Word -> Tag -> Tag -> Prob
joint tables gamma word nextTag currTag =
    condProb word currTag (observe tables) *
    condProb nextTag currTag (transition tables) *
    gammaFind currTag gamma

marg :: Tables -> Gamma -> Word -> Tag -> Prob
marg tables gamma word nextTag = sum (map join tags)
    where join = joint tables gamma word nextTag

buildG2 :: Tables -> Word -> Gamma -> Gamma2
buildG2 tables word gamma = allTags (\t -> makeInference $
                                     \s -> joint tables gamma word t s)

rightBuild :: Tables -> Gamma2 -> Sentence -> Gamma2
rightBuild tables initG2 right = S.foldlWithIndex (go rlen) initG2 right
    where rlen = S.length right
          go last g2 i wrd
              | i == last = close (inferSum final)
              | otherwise = allTags (\next -> inferSum (interm next))
              where inferSum addF = M.foldWithKey addF (return 0) g2
                    final curr inf = inferAdd (inferScale (wrdProb curr) inf)
                    wrdProb curr = condProb wrd curr (observe tables)
                    interm nxt cur inf =
                        inferAdd (inferScale (wrdProb cur * trns nxt cur) inf)
                    trns next curr = condProb next curr (transition tables)
