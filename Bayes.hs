module Bayes ( bayes ) where

-- | Importing external modules/libraries
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List (foldl')
import qualified Data.Foldable as F    
import qualified Control.Monad.Trans.Reader as R

-- | Importing internal modules written for this project    
import Types

type Gamm a = M.Map Tag a

allTags :: (Tag -> a) -> Gamm a
allTags f = foldl' (\g t -> M.insert t (f t) g) M.empty tags

dummyTag :: Tag
dummyTag = PUNC
            
close :: a -> Gamm a
close a = M.singleton dummyTag a

type Gamma = Gamm Prob    

fromPT :: PT Tag -> Gamma
fromPT pt = M.mapWithKey (\k _ -> prob k pt) (counts pt)

type Inference = R.Reader Tag Prob
    
type Gamma2 = Gamm Inference

retrieve :: Gamma2 -> Inference
retrieve = M.findWithDefault (return 0) dummyTag

infer :: (Tag -> Prob) -> Inference
infer = R.reader    

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
         
gammaFind :: Tag -> Gamma -> Prob
gammaFind = M.findWithDefault 0

forEachTag :: M.Map Tag a -> (Tag -> a -> b) -> M.Map Tag b
forEachTag m f = M.mapWithKey f m

bayes :: Tables -> Sentence -> TaggedSent
bayes tables sentence = S.mapWithIndex tagWord sentence
    where tagWord i word = 
              let (left, right) = except i sentence
                  leftGamm = F.foldl' leftBuild (fromPT (start tables)) left
                  gamm2 = buildG2 word leftGamm
                  
                  -- c tag = condProb word tag (observe tables)
                  -- rate tag = (tag, c tag * gammaFind tag gamma)
              in undefined -- (word, bestTag (map rate tags))
          leftBuild gamm wrd = foldl' (newGamm wrd) gamm tags
          newGamm wrd old t = M.insert t (margCurr tables old wrd t) old
          buildG2 wrd gamm = allTags (\t -> infer $
                                      \s -> condProb wrd s (observe tables) *
                                            condProb t s (transition tables) *
                                            gammaFind s gamm)
          rightBuild last g2 i wrd
              | i == last = close (inferSum final)
              | otherwise = allTags (\next -> inferSum (interm next))
              where inferSum f = M.foldWithKey f (return 0) g2
                    final curr inf = inferAdd (inferScale (wrdProb curr) inf)
                    wrdProb curr = condProb wrd curr (observe tables)
                    interm nxt cur inf =
                        inferAdd (inferScale (wrdProb cur * trns nxt cur) inf)
                    trns next curr = condProb next curr (transition tables)
              -- let trans next curr = if i == last then 1
              --                       else condProb next curr (transition tables)
              -- in allTags (\next -> forEachTag g2
              --                      (\curr inf ->
              --                           inferScale
              --                           (condProb wrd curr (observe tables))
              --                           (inferScale (trans next curr) inf)))

jointCurr :: Tables -> Gamma -> Word -> Tag -> Tag -> Prob
jointCurr tables gamma word nextTag currTag =
    condProb word currTag (observe tables) *
    condProb nextTag currTag (transition tables) *
    gammaFind currTag gamma

margCurr :: Tables -> Gamma -> Word -> Tag -> Prob
margCurr tables gamma word nextTag = sum (map joint tags)
    where joint = jointCurr tables gamma word nextTag

jointNext :: Tables -> Gamma -> Word -> Tag -> Tag -> Prob
jointNext tables gamma word currTag nextTag =
    condProb word nextTag (observe tables) *
    condProb nextTag currTag (transition tables) *
    gammaFind currTag gamma

margNext :: Tables -> Gamma -> Word -> Tag -> Prob
margNext tables gamma word currTag = sum (map joint tags)
    where joint = jointNext tables gamma word currTag
   
-- Shouldn't j == 0 be calling start tables somewhere?
-- Perhaps init gamma should be equal to start
