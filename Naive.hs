module Naive where

-- | Importing external modules/libraries    
import Data.List (maximumBy)
import qualified Data.Foldable as F
import qualified Data.Sequence as S

-- | Importing internal modules written for this project    
import Types    

naive :: Tables -> Sentence -> Mem Word TaggedSent
naive tables sent = S.foldlWithIndex build initial sent
  where initial = return emptySent
        build mem i word = do taggedsent <- mem
                              tword <- tagged word i
                              return $ extend taggedsent tword
        tagged word i = do
          let others = S.splitAt i sent
              bind mp w = mp >>= memProdMarg tables w
          c <- unto others bind (return 1)
          let rate tag = (tag, c * joint tables word tag)
          return (word, bestTag $ map rate tags)

-- | p(Si, Wi) = p(Wi|Si) * p(Si)
joint :: Tables -> Word -> Tag -> Prob
joint tables word tag = condProb word tag (observe tables)
                        * prob tag (marginal tables)

-- | Marginalize a word over all possible tags
-- p(Wi) = \Sum_t p(Wi, Si=t)
wordMarg :: Tables -> Word -> Prob
wordMarg tables word = sum $ map (joint tables word) tags
    
-- | Here "best" means "most probable", given a list of (Tag, prob) pairs
bestTag :: [(Tag, Prob)] -> Tag
bestTag = fst . maximumBy higherProb
    where higherProb (tag1,p1) (tag2,p2) = compare p1 p2               

-- | Memoize the marginalization of a word, and compute product marginal
-- q * memoized p(Wi), where q is an accumulating product of probabilities
memProdMarg :: Tables -> Word -> Prob -> Mem Word Prob
memProdMarg tables word q = do p <- memoize (wordMarg tables) word
                               return (q*p)
