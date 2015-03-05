module Bayes ( bayes ) where

-- | Importing external modules/libraries
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List (foldl')
import qualified Data.Foldable as F    
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as St -- ^ State monad
import qualified Data.Maybe as MB
import qualified Control.Monad as CM

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
    
bayes :: Tables -> Sentence -> TaggedSent
bayes tables sentence = evalWith initMem memoTag
    where initMem = M.singleton 0 (fromStart tables)
          memoTag = S.foldlWithIndex build (return emptySent) sentence
          build mem i word = do taggedSent <- mem
                                taggedWord <- tagWord word i
                                return (extend taggedSent taggedWord)
          tagWord word i = do
            prevGamm <- storeGamma (S.length sentence) i tables word
            let initG2 = buildG2 tables word prevGamm
                (_, right) = except i sentence
                finalG2 = rightBuild tables initG2 right
                probTag = retrieve finalG2
                rate tag = (tag, infer probTag tag)
            return (word, bestTag (map rate tags))

storeGamma :: Int -> Int -> Tables -> Word -> Mem Int Gamma Gamma
storeGamma len i tables word = do
  gMap <- St.get
  let prevGamm = MB.fromJust $ M.lookup i gMap
      currGamm = newGamm tables prevGamm word
  CM.when (i < len-1) $ St.put (M.insert (i+1) currGamm gMap)
  return prevGamm

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
