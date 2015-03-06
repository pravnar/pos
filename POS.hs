module POS where

-- | Importing external modules/libraries
import System.Environment
import qualified System.Random.MWC as MWC    

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
  let tables = learn Start (lines training) emptyTables
      pDet = prob DET (start tables)
  print pDet

        
testing :: Tables -> [String] -> Sentence -> [Tag] -> IO ()
testing tables (wordAndTag : rest) sentence answers =
    if null wordAndTag
    then testSent tables sentence (reverse answers) >>
         testing tables rest emptySent []
    else do let (word,tag) = parse wordAndTag
            testing tables rest (extend sentence word) (tag:answers)

testSent :: Tables -> Sentence -> [Tag] -> IO ()                    
testSent tables sentence answers = do
  rand <- MWC.createSystemRandom
  
        
