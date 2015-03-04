module Naive where

-- | Importing external modules/libraries
import qualified Data.Sequence as S

-- | Importing internal modules written for this project    
import Types    

naive :: Tables -> Sentence -> Mem Word TaggedSent
naive tables sentence = S.foldlWithIndex build initial sentence
  where initial = return emptySent
        build mem i word = do taggedSent <- mem
                              taggedWord <- tagged word i
                              return (extend taggedSent taggedWord)
        tagged word i = do
          let otherWords = S.splitAt i sentence
          c <- unto otherWords (productMarg tables) (return 1)
          let rate tag = (tag, c * joint tables word tag)
          return (word, bestTag (map rate tags))

-- | p(Si, Wi) = p(Wi|Si) * p(Si)
joint :: Tables -> Word -> Tag -> Prob
joint tables word tag = condProb word tag (observe tables)
                        * prob tag (marginal tables)

-- | Marginalize a word over all possible tags
-- p(Wi) = \Sum_t p(Wi, Si=t)
wordMarg :: Tables -> Word -> Prob
wordMarg tables word = sum (map (joint tables word) tags)
    
-- | Compute memoized product of marginals
-- q * P(Wi), where q is an accumulating product of probabilities
productMarg :: Tables -> Mem Word Prob -> Word -> Mem Word Prob
productMarg tables accum w = memProduct accum (memoize (wordMarg tables) w)


