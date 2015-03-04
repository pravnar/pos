module Bayes where

-- | Importing external modules/libraries
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List (foldl')    

-- | Importing internal modules written for this project    
import Types

type Gamma = MemTable Tag

gammFind :: Tag -> Gamma -> Prob
gammFind = M.findWithDefault 1

bayes :: Tables -> Sentence -> TaggedSent
bayes tables sentence = S.mapWithIndex tagWord sentence
    where tagWord i word =
              let gamma = S.foldlWithIndex (buildGamm i) emptyMemT sentence
                  c tag = condProb word tag (observe tables)
                  rate tag = (tag, c tag * gammFind tag gamma)
              in (word, bestTag (map rate tags))
          buildGamm i gamma j wordj
              | i > j  = foldl' (newGamm margCurr wordj) gamma tags
              | i == j = gamma
              | i < j  = foldl' (newGamm margNext wordj) gamma tags
          newGamm marg wrd old t = M.insert t (marg tables old wrd t) old

jointCurr :: Tables -> Gamma -> Word -> Tag -> Tag -> Prob
jointCurr tables gamma word nextTag currTag =
    condProb word currTag (observe tables) *
    condProb nextTag currTag (transition tables) *
    gammFind nextTag gamma

margCurr :: Tables -> Gamma -> Word -> Tag -> Prob
margCurr tables gamma word nextTag = sum (map joint tags)
    where joint = jointCurr tables gamma word nextTag

jointNext :: Tables -> Gamma -> Word -> Tag -> Tag -> Prob
jointNext tables gamma word currTag nextTag =
    condProb word nextTag (observe tables) *
    condProb nextTag currTag (transition tables) *
    gammFind currTag gamma

margNext :: Tables -> Gamma -> Word -> Tag -> Prob
margNext tables gamma word currTag = sum (map joint tags)
    where joint = jointNext tables gamma word currTag
   
