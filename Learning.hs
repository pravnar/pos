module Learning ( learn ) where

-- | Importing external modules/libraries    
import qualified Data.Map as M  -- ^ For maps / dictionaries

-- | Importing internal modules written for this project
import Types                        
                
learn :: Position -> [String] -> Tables -> Tables
learn _ [] tables = tables {- reached end of list; return tables unchanged -}
learn pos (wordAndTag : rest) tables =
    if null wordAndTag           {- empty line between sentences -}
    then learn Start rest tables {- begin reading new sentence -}
    else let (word, tag) = parse wordAndTag
             firstUpdate = case pos of
                             Start -> updateStart tag
                             Trans prevTag -> updateTrans prevTag tag
             {- dot means function composition -}
             update = (updateMarg tag) . (updateObs tag word) . firstUpdate
         in learn (Trans tag) rest (update tables) {- update probs; continue -}

-- | Update the start-tags-CPT while leaving the other tables unchanged
updateStart :: Tag -> Tables -> Tables
updateStart tag tables = tables { start = increment tag (start tables) }

-- | Update the tags-transitions-CPT while leaving the other tables unchanged 
updateTrans :: Tag -> Tag -> Tables -> Tables
updateTrans prevTag tag tables = tables { transition = t (transition tables) }
    where t = M.insertWith' (incr2 tag) prevTag (singleton tag)

-- | Update the word-tags-CPT while leaving the other tables unchanged
updateObs :: Tag -> Word -> Tables -> Tables
updateObs tag word tables = tables { observe = o (observe tables) }
    where o = M.insertWith' (incr2 word) tag (singleton word)    

-- | Update the marginal-tags-CPT while leaving the other tables unchanged
updateMarg :: Tag -> Tables -> Tables
updateMarg tag tables = tables { marginal = increment tag (marginal tables) }
