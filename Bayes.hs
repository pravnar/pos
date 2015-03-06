module Bayes where

-- | Importing external modules/libraries
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List (foldl')
import qualified Data.Foldable as F    
import qualified Control.Monad.Trans.State as St -- ^ State monad
import qualified Data.Maybe as MB

-- | Importing internal modules written for this project    
import Types

type Gamma = MemTable Tag Prob

-- Helper functions on Gammas
--------------------------------------------------------------------------------    
emptyGamma :: Gamma
emptyGamma = emptyMemT

allTags :: (Tag -> Prob) -> Gamma
allTags f = foldl' (\g t -> M.insert t (f t) g) M.empty tags
    
fromPT :: PT Tag -> Gamma
fromPT pt = M.mapWithKey (\k _ -> prob k pt) (counts pt)

fromStart :: Tables -> Gamma
fromStart tables = fromPT (start tables)

gammaFind :: Tag -> Gamma -> Prob
gammaFind = M.findWithDefault 0
--------------------------------------------------------------------------------

type FBTable = MemTable Int Gamma
                                     
data FB = FB { forTable :: FBTable
             , backTable :: FBTable }

type MemoFB a = St.State FB a

newFB :: Tables -> FB
newFB tables = FB (M.singleton 0 (fromStart tables)) emptyMemT

eval :: Tables -> MemoFB a -> a
eval tables m = St.evalState m (newFB tables)

bayes :: Tables -> Sentence -> TaggedSent
bayes tables sentence = eval tables memoTag
    where memoTag = S.foldlWithIndex build (return emptySent) sentence
          build mem i word = do taggedSent <- mem
                                taggedWord <- tagWord word i
                                return (extend taggedSent taggedWord)
          tagWord word i = do
            let (_,right) = except i sentence
            forGamma <- forward tables word i
            backGamma <- checkFirst (backward tables right) i
            let rate t = (t, gammaFind t backGamma * gammaFind t forGamma)
            return (word, bestTag (map rate tags))
                                     
backward :: Tables -> Sentence -> Int -> MemoFB Gamma
backward tables right i = do
  let initG = allTags (const 1)
      build j word mem = do
        fbTables <- St.get
        gamma <- mem
        let backGamms  = backTable fbTables
            backGamms' = M.insert (i+j+1) gamma backGamms
            gamma' = allTags
                     (\lower -> M.foldWithKey
                                (backAdd tables word lower) 0 gamma)
        St.put (fbTables {backTable = backGamms'})
        return gamma'
  S.foldrWithIndex build (return initG) right

backAdd :: Tables -> Word -> Tag -> Tag -> Prob -> Prob -> Prob
backAdd tables word lower curr value total = total + value * backJoint
    where backJoint = condProb word curr (observe tables) *
                      condProb curr lower (transition tables)

checkFirst :: (Int -> MemoFB Gamma) -> Int -> MemoFB Gamma
checkFirst f i = do
  fbTables <- St.get
  let val = M.lookup i (backTable fbTables)
  if MB.isJust val then return (MB.fromJust val) else f i
     
forward :: Tables -> Word -> Int -> MemoFB Gamma
forward tables word i = do
  fbTables <- St.get
  let forGamms = forTable fbTables
      gamma = MB.fromJust $ M.lookup i forGamms
      forNew = allTags
               (\higher -> M.foldWithKey
                           (forAdd tables word higher) 0 gamma)
      forGamms' = M.insert (i+1) forNew forGamms
  St.put (fbTables {forTable = forGamms'})
  return forNew

forAdd :: Tables -> Word -> Tag -> Tag -> Prob -> Prob -> Prob
forAdd tables word higher curr value total = total + value * forJoint
    where forJoint = condProb word curr (observe tables) *
                     condProb higher curr (transition tables)
                              
