module Types where

-- | Importing external modules/libraries    
import qualified Data.Map as M  -- ^ For maps / dictionaries
import qualified Data.Text as T -- ^ Efficient string representations
import qualified Data.Sequence as S -- ^ For representing sentences
import qualified Data.Foldable as F
import qualified Control.Monad.Trans.State as St -- ^ State monad
import Data.Maybe
import Data.List (maximumBy)
import Data.Ord
import qualified Data.Number.LogFloat as LF -- ^ For log probabilities

-- | Enum type of POS tags    
data Tag = ADJ | ADV  | ADP | CONJ
         | DET | NOUN | NUM | PRON
         | PRT | VERB | X   | PUNC
           deriving (Eq, Ord)

-- | Specify conversion of Strings to Tags
instance Read Tag where
    readsPrec _ s
        | s == "ADJ"  = [(ADJ  , "")]
        | s == "ADV"  = [(ADV  , "")]
        | s == "ADP"  = [(ADP  , "")]
        | s == "CONJ" = [(CONJ , "")]
        | s == "DET"  = [(DET  , "")]
        | s == "NOUN" = [(NOUN , "")]
        | s == "NUM"  = [(NUM  , "")]
        | s == "PRON" = [(PRON , "")]
        | s == "PRT"  = [(PRT  , "")]
        | s == "VERB" = [(VERB , "")]
        | s == "X"    = [(X    , "")]
        | s == "."    = [(PUNC , "")]

-- | Specify conversion of Tags to Strings 
instance Show Tag where
    show ADJ  = "ADJ"
    show ADV  = "ADV"
    show ADP  = "ADP"
    show CONJ = "CONJ"
    show DET  = "DET"
    show NOUN = "NOUN"
    show NUM  = "NUM"
    show PRON = "PRON"
    show PRT  = "PRT" 
    show VERB = "VERB"
    show X    = "X"
    show PUNC = "."

tags :: [Tag]
tags = [ADJ, ADV, ADP, CONJ, DET, NOUN, NUM, PRON, PRT, VERB, X, PUNC]

newtype Word = W T.Text
    deriving (Eq, Ord)

instance Show Word where
    show (W t) = T.unpack t

parse :: String -> (Word, Tag)
parse wordAndTag = (W $ T.pack word, read tag)
    where [word, tag] = words wordAndTag                         

-- | Defining a sentence as a sequence
type Sent = S.Seq

-- Helper functions on Sents
--------------------------------------------------------------------------------
emptySent :: Sent a
emptySent = S.empty

-- | Add the element to the end of the sentence
extend :: Sent a -> a -> Sent a
extend sent e = sent S.|> e

except :: Int -> Sent a -> (Sent a, Sent a)
except i sent = (before, after)
    where (before, rest) = S.splitAt i sent
          after = case S.viewl rest of
                    S.EmptyL -> rest
                    curr S.:< remaining -> remaining

unto :: (Sent b, Sent b) -> (a -> b -> a) -> a -> a
unto (before, after) f init = F.foldl' f (F.foldl' f init before) after

-- | Assume non empty sentence                              
getLast :: Sent a -> a
getLast sentence = case S.viewr sentence of
                     S.EmptyR -> error "Assumed non empty sentence in getPrev"
                     (_ S.:> last) -> last
--------------------------------------------------------------------------------

type Sentence = Sent Word
type TaggedSent = Sent (Word, Tag)

makeSent :: [Word] -> Sentence
makeSent = S.fromList

-- | A datatype that tells us whether we:
-- 1. have started a new sentence, or
-- 2. are in the middle of a sentence with the preceding tag in hand
data Position = Start | Trans Tag

-- | Defining probabilities as double-precision floating point numbers
-- in the log-domain
type Prob = LF.LogFloat

-- | Here "best" means "most probable", given a list of (Tag, Prob) pairs
bestTag :: [(Tag, Prob)] -> Tag
bestTag = fst . maximumBy (comparing snd)
    
-- | Defining probability tables
-- The parameter "a" represents a generic type
data PT a = PT { counts :: M.Map a Int -- ^ Map from objects to counts
               , total :: Int }        -- ^ Sum of counts in the map

-- Helper functions on PTs           
--------------------------------------------------------------------------------
emptyPT :: PT a
emptyPT = PT M.empty 0

-- | Update the PT either by
-- 1. adding a new object (with count 1) to the map, or
-- 2. incrementing the count of the object if it already exists in the map
-- Also increment the total counts in the PT
increment :: (Ord a) => a -> PT a -> PT a
increment key pt = PT (M.insertWith' (+) key 1 (counts pt)) (total pt + 1)

singleton :: (Ord a) => a -> PT a
singleton key = increment key emptyPT

-- | To get the probability of an object, lookup its count value in the map
-- and divide it by the total counts
prob :: (Ord a) => a -> PT a -> Prob
prob key pt = if val == 0 then LF.logFloat 0
              else LF.logFloat (fromIntegral val / fromIntegral (total pt))
    where val = M.findWithDefault 0 key (counts pt)

-- | Ignore the first PT and increment (i.e., update) the second one
incr2 :: (Ord a) => a -> PT a -> PT a -> PT a
incr2 key _ pt = increment key pt
--------------------------------------------------------------------------------

-- | Defining conditional probability tables
type CPT a b = M.Map b (PT a)

-- | P(a|b)
condProb :: (Ord a, Ord b) => a -> b -> CPT a b -> Prob
condProb a b cpt = prob a (M.findWithDefault emptyPT b cpt)

-- | Defining a structure that collects various probability tables
data Tables = T { start      ::  PT Tag      -- ^ P ( S1 )
                , transition :: CPT Tag Tag  -- ^ P ( S(i+1) | Si )
                , observe    :: CPT Word Tag -- ^ P ( Wi | Si )
                , marginal   ::  PT Tag }    -- ^ P ( Si ) for all i

emptyTables :: Tables
emptyTables = T emptyPT M.empty M.empty emptyPT

-- | Defining memotables for storing computed values
type MemTable = M.Map

emptyMemT :: MemTable a b
emptyMemT = M.empty

-- | Computations can have an associated memotable (a -> b)
-- The return type of the computation is c
-- Using the State monad to express this allows us to update and pass
-- along memotables in an imperative style
type Mem a b c = St.State (MemTable a b) c

-- Helper functions on Mems        
--------------------------------------------------------------------------------
-- | Memoizing functions that take one argument
memoize :: (Ord a) => (a -> b) -> a -> Mem a b b
memoize f a = do
  memo <- St.get {- the current memotable -}
  let val = M.lookup a memo
  if (isJust val) {- the value already exists in the table -}
  then return (fromJust val) {- return value with memotable unchanged -}
  else do
    let result = f a {- do the computation -}
    St.put (M.insert a result memo) {- update memotable with new result -}
    return result {- return new result and updated memotable -}

-- | Evaluate a memoized computation, starting with empty memotable
memRun :: Mem a b c -> MemTable a b -> (c, MemTable a b)
memRun m table = St.runState m table

-- | Compute the product of two memoized probability computations               
memProduct :: Mem a b Prob -> Mem a b Prob -> Mem a b Prob
memProduct mp1 mp2 = do
  p1 <- mp1
  p2 <- mp2
  return (p1*p2)
--------------------------------------------------------------------------------
