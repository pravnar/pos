module POS where

-- | Importing external modules/libraries
import System.Environment

-- | Importing internal modules written for this project
import Types 
import Learning

main :: IO ()
main = do
  (trainFile : testFile : _) <- getArgs
  training <- readFile trainFile
  let tables = learn Start (lines training) emptyTables
      pDet = prob DET (start tables)
  print pDet
