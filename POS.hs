module POS where

-- | Importing external modules/libraries
import System.Environment
import qualified System.Random.MWC as MWC
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Control.Monad as C
import Data.List (maximumBy)
import Data.Ord    

-- | Importing internal modules written for this project
import Types 
import Learning
import Naive
import Bayes
import Sampling

main :: IO ()
main = do
  (trainFile : testFile : _) <- getArgs
  training <- readFile trainFile
  testing <- readFile testFile

  let tables = learn Start (lines training) emptyTables
      testlines = lines testing  

  -- naiveStats <- step2 tables testlines

  -- putNewLine
  -- putStrLn "PERFORMANCE SUMMARY"
  -- putStrLn $ "Total words: " ++ show (numwords naiveStats) ++
  --              " sentences: " ++ show (numsents naiveStats)
  -- display "Part 2, Naive Graphical Model" naiveStats

  -- bayesStats <- step3 tables testlines
  -- display "Part 3, Bayes net" bayesStats

  sampleStats <- step4 tables testlines
  display "Part 4, Sampling" sampleStats
  
  putNewLine

data Stats = S { correctsents :: Int
               , numsents :: Int
               , correctwords :: Int
               , numwords :: Int } deriving (Show)

emptyStats :: Stats
emptyStats = S 0 0 0 0

-- | Expects the list of tags (answers) to be backwards
newStats :: Stats -> [Tag] -> TaggedSent -> Stats
newStats stats revanswers taggedSent = S { correctsents = cs
                                         , numsents = ns + 1
                                         , correctwords = cw
                                         , numwords = nw }
    where guesses = getGuesses taggedSent
          ns = numsents stats
          nw = numwords stats + S.length taggedSent
          answers = reverse revanswers
          cs = if guesses == answers
               then (correctsents stats + 1) else correctsents stats
          cw = correctwords stats + numRight answers guesses

getGuesses :: TaggedSent -> [Tag]
getGuesses taggedSent = snd (unzip (F.toList taggedSent))
               
numRight :: [Tag] -> [Tag] -> Int
numRight answers guesses = length (filter isRight (zip answers guesses))

isRight :: (Tag, Tag) -> Bool
isRight (answer, guess) = answer == guess

-- | Get ratios from total counts                      
compute :: Stats -> (Double, Double)
compute stats = ( f cs / f ns
                , f cw / f nw )
    where cs = correctsents stats
          ns = numsents stats
          cw = correctwords stats
          nw = numwords stats
          f = fromIntegral

-- | Round a floating point number up to n significant digits
roundN :: Int -> Double -> Double
roundN n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

toPercent :: Double -> String
toPercent f = show (f*100) ++ "%"

putNewLine :: IO ()
putNewLine = putStrLn ""

display :: String -> Stats -> IO ()
display part stats = do
  putNewLine
  putStrLn part
  putStrLn "-------"
  putStrLn $ "Words correct: " ++ convert wordsRatio
  putStrLn $ "Sentences correct: " ++ convert sentsRatio
  where convert = toPercent . roundN 6
        (sentsRatio, wordsRatio) = compute stats        
        
step2 :: Tables -> [String] -> IO Stats
step2 tables testfile = go emptyMemT testfile emptySent [] emptyStats
    where go _ [] _ _ stats = return stats
          go memo (wordAndTag : rest) sentence answers stats
              = if null wordAndTag
                then do
                  let (tagged, memo') = memRun (naive tables sentence) memo
                  go memo' rest emptySent [] (newStats stats answers tagged)
                else do
                  let (word,tag) = parse wordAndTag
                  go memo rest (extend sentence word) (tag:answers) stats

step3 :: Tables -> [String] -> IO Stats
step3 tables testfile = go testfile emptySent [] emptyStats
    where go [] _ _ stats = return stats
          go (wordAndTag : rest) sentence answers stats
              = if null wordAndTag
                then do
                  tagged <- bayes tables sentence
                  putStrLn $ "tagged: " ++ show tagged
                  putStrLn $ "answers: " ++ show (reverse answers)
                  go rest emptySent [] (newStats stats answers tagged)
                else do
                  let (word,tag) = parse wordAndTag
                  go rest (extend sentence word) (tag:answers) stats
                                    
step4 :: Tables -> [String] -> IO Stats
step4 tables testfile = MWC.createSystemRandom >>=
                        go testfile emptySent [] emptyStats
    where go [] _ _ stats _ = return stats
          go (wordAndTag : rest) sentence answers stats rand
              = if null wordAndTag
                then do
                  tSents <- C.replicateM 5 (sampling tables sentence rand)
                  -- putStrLn $ "samples: " ++ show tSents
                  -- putStrLn $ "answers: " ++ show (reverse answers)
                  let bestSent = bestSample tSents answers
                  -- print bestSent
                  -- print (reverse answers)
                  go rest emptySent [] (newStats stats answers bestSent) rand
                else do
                  let (word,tag) = parse wordAndTag
                  go rest (extend sentence word) (tag:answers) stats rand

-- | Expects the list of tags (answers) to be backwards
bestSample :: [TaggedSent] -> [Tag] -> TaggedSent             
bestSample taggedSents revanswers = byWords bySent
    where answers = reverse answers
          checkEqual sent = (getGuesses sent) == answers
          completely = filter checkEqual taggedSents
          bySent = taggedSents -- if null completely then taggedSents else completely
          nRight sent = numRight answers (getGuesses sent)
          byWords = head -- maximumBy (comparing nRight)
