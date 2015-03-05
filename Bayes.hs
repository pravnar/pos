module Bayes ( bayes ) where

-- | Importing external modules/libraries
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List (foldl')
import qualified Data.Foldable as F    
import qualified Control.Monad.Trans.Reader as R

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

type Gamma = Gamm Prob    

-- Helper functions on Gammas
--------------------------------------------------------------------------------    
fromPT :: PT Tag -> Gamma
fromPT pt = M.mapWithKey (\k _ -> prob k pt) (counts pt)

gammaFind :: Tag -> Gamma -> Prob
gammaFind = M.findWithDefault 0
--------------------------------------------------------------------------------
            
type Inference = R.Reader Tag Prob
    
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
leftBuild tables left = F.foldl' newGamm startGamm left
    where startGamm = fromPT (start tables)
          newGamm gamm word = foldl' (gamm' word) gamm tags
          gamm' word old tag = M.insert tag (marg tables old word tag) old

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
