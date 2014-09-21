module Coloring (
 ) where

import Data.Maybe
import Data.List
import Data.Ord
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Dot
import System.Process
import Control.Monad.State

import Bee

test = runElla

test1 =  (O Add (O Add (O Add (N 1) (N 2)) (N 3)) (N 4))
test2 =  (O Add (N 1) (O Add (N 2) (O Add (N 3) (N 4))))
test3 =  (O Add (O Add test2 test1) (N 1))

-- AMov (Cell 0) (Num 4)       |
-- AMov (Cell 1) (Num 3)       | |
-- AMov (Cell 2) (Num 2)       | | |
-- AMov (Register ()) (Num 1)  | | |
-- AAdd (Register ()) (Cell 2) | | |
-- AAdd (Register ()) (Cell 1) | |
-- AAdd (Register ()) (Cell 0) |

-- AMov (Cell 2) (Num 4)        |
-- AMov (Register ()) (Num 3)   |
-- AAdd (Register ()) (Cell 2)  |
-- AMov (Cell 1) (Register ())   |
-- AMov (Register ()) (Num 2)    |
-- AAdd (Register ()) (Cell 1)   |
-- AMov (Cell 0) (Register ())    |
-- AMov (Register ()) (Num 1)     |
-- AAdd (Register ()) (Cell 0)    |

type Lifetime = (Int, Int)

meet :: Lifetime -> Lifetime -> Bool
meet (p,q) (s,t) =
 if p <= s
    then s <= q
    else p <= t

livenessAnalysis :: Int -> [A ()] -> [(Cell, Lifetime)]
livenessAnalysis _ [] = []
livenessAnalysis n (x:xs) = birth n x xs ++ livenessAnalysis (n+1) xs
 where birth n (AMov (Cell i) _) rest = death n (n+1) i rest
       birth n _ _ = []
       death m n i (AAdd _ (Cell j):_) | i == j = [(i,(m,n))]
       death m n i (AAdd _ _:rest) = death m (n+1) i rest
       death m n i (ASub _ (Cell j):_) | i == j = [(i,(m,n))]
       death m n i (ASub _ _:rest) = death m (n+1) i rest
       death m n i (AMul (Cell j):_) | i == j = [(i,(m,n))]
       death m n i (AMul _:rest) = death m (n+1) i rest
       death m n i (AMov (Cell j) _:_) | i == j = [(i,(m,n))]
       death m n i (AMov _ _:rest) = death m (n+1) i rest
       death m n i [] = [(i,(m,n))]

-- *Coloring> livenessAnalysis 0 test1
-- [(0,(0,6)),(1,(1,5)),(2,(2,4))]
-- *Coloring> livenessAnalysis 0 test2
-- [(2,(0,2)),(1,(3,5)),(0,(6,8))]

livenessGraph :: [(Node, Lifetime)] -> Gr Lifetime ()
livenessGraph analysis = mkGraph nodes edges where
 nodes = analysis
 edges = concatMap overlaps nodes
 overlaps (i,life) = map (\(j, _) -> (i, j, ())) .
                     filter (meet life . snd) .
                     filter (\(j,_) -> j /= i) $
                     analysis

coloring graph = execState (mapM colorVertex heuristic) [] where
 -- sort the vertices of the graph in order of most edges first
 heuristic = sortBy (flip (comparing (deg graph))) . nodes $ graph
 colorVertex i = do let ne = neighbors graph i
                    colors <- get
                    let neColors = catMaybes . map (flip lookup colors) $ ne
                    let myColor = head $ [1..] \\ neColors
                    put ((i,myColor):colors)

graphColoring graph = gmap color graph where
 color (l, i, _, r) = (l, i, fromJust . lookup i $ colors, r)
 colors = coloring graph

runTest test = do
    let dot = showDot  . fglToDot . graphColoring . livenessGraph . livenessAnalysis 0 $ test
    writeFile "file.dot" dot
    system("dot -Tpng -ofile.png file.dot")

