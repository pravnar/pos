module Naive ( naive ) where

-- | Importing external modules/libraries
import qualified Data.Sequence as S
import qualified Data.Number.LogFloat as LF

-- | Importing internal modules written for this project    
import Types

-- | Memoized computations that store (a -> Prob) tables
type MemProb a c = Mem a Prob c

-- As an example, MemoProb Word TaggedSent is a memoized
-- computation that returns TaggedSent objects, and stores
-- behind-the-scenes a Word -> Prob memotable
naive :: Tables -> Sentence -> MemProb Word TaggedSent
naive tables sentence = S.foldlWithIndex build initial sentence
  where initial = return emptySent
        build mem i word = do taggedSent <- mem
                              taggedWord <- tagged word i
                              return (extend taggedSent taggedWord)
        tagged word i = do
          let otherWords = except i sentence
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
wordMarg tables word = LF.sum (map (joint tables word) tags)
    
-- | Compute memoized product of marginals
-- q * P(Wi), where q is an accumulating product of probabilities
productMarg :: Tables -> MemProb Word Prob -> Word -> MemProb Word Prob
productMarg tables accum w = memProduct accum (memoize (wordMarg tables) w)


