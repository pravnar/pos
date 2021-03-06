module Tests where

-- | Importing external modules/libraries
import System.Environment
import qualified Data.Map as M  -- ^ For maps / dictionaries
import qualified Data.Text as T -- ^ Efficient string representations
import qualified Data.Sequence as S
import qualified System.Random.MWC as MWC
import qualified Control.Monad as CM
    
-- | Importing internal modules written for this project
import Types
import Learning
import Naive
import Bayes
import Sampling    

t1 :: IO ()
t1 = do
  let wt = parse "\" ."
  print wt

t2 :: IO ()
t2 = do
  file <- readFile "test.txt"
  let render s = if null s then putStrLn $ s ++ "<-- empty string here"
                 else putStrLn s
  mapM_ render (lines file)

t3 :: IO ()
t3 = do
  file <- readFile "test.txt"
  let go i (x:xs) = if i == 10 then putStrLn "Done!"
                    else putStrLn x >> go (i+1) xs
      go _ [] = putStrLn "Empty list"
  go 0 (lines file)

t4 :: IO ()
t4 = do
  file <- readFile "test.txt"
  let render s = if null s then putStrLn $ s ++ "<-- empty string here"
                 else print $ parse s
  mapM_ render (lines file)
        
t5 :: IO ()
t5 = do
  (trainFile : testFile : _) <- getArgs
  training <- readFile trainFile
  let tables = learn Start (lines training) emptyTables
      pDet = prob DET (marginal tables)
      pNoun = prob NOUN (marginal tables)
  putStrLn $ "p(Det) = " ++ show pDet
  putStrLn $ "p(Noun) = " ++ show pNoun

t6 :: IO ()
t6 = print.bestTag $ [(NOUN,0.4), (DET,0.2), (ADJ,0.1), (ADV,0.1), (PRT,0.2)]

t7 :: IO ()
t7 = do
  (trainFile : testFile : _) <- getArgs
  training <- readFile trainFile
  let tables = learn Start (lines training) emptyTables
      makeTest = makeSent . map (W . T.pack)
      test = makeTest ["The", "position", "covers", "tasks", "."]
      test2 = makeTest ["If", "the", "covers", "were", "no", "problem", "."]
      (result, t1) = memRun (naive tables test) emptyMemT
      (r2, _) = memRun (naive tables test2) t1
  print $ "Size of marginal:" ++ show (total (marginal tables))
  print result
  print r2

t8 :: IO ()
t8 = do
  (trainFile : testFile : _) <- getArgs
  training <- readFile trainFile
  let tables = learn Start (lines training) emptyTables
      makeTest = makeSent . map (W . T.pack)
      test = makeTest ["The", "position", "covers", "tasks", "."]
      test2 = makeTest ["If", "the", "covers", "were", "no", "problem", "."]      
  result <- bayes tables test
  r2 <- bayes tables test2
  print $ "Size of marginal:" ++ show (total (marginal tables))
  print result
  print r2

t9 :: IO ()
t9 = do
  (trainFile : testFile : _) <- getArgs
  training <- readFile trainFile
  let tables = learn Start (lines training) emptyTables
      makeTest = makeSent . map (W . T.pack)
      test = makeTest ["The", "position", "covers", "tasks", "."]
      test2 = makeTest ["If", "the", "covers", "were", "no", "problem", "."]
  g <- MWC.createSystemRandom
  result <- sampling tables test g
  r2 <- sampling tables test2 g
  print $ "Size of marginal:" ++ show (total (marginal tables))
  print result
  print r2

ratedTags = [(ADJ,0.0),(ADV,0.0),(ADP,0.0),(CONJ,0.0)
            ,(DET,0.0),(NOUN,4.524150237313932e-24),(NUM,0.0),(PRON,0.0)
            ,(PRT,0.0),(VERB,5.598151716460531e-24),(X,0.0),(PUNC,0.0)]

t10 :: IO ()
t10 = do
  g <- MWC.createSystemRandom
  let dist = categoricalNormed ratedTags
  samples <- CM.replicateM 5 $ sampleFrom dist g
  print samples
        
