module Coloring (
 livenessAnalysis, livenessGraph, graphColoring,
 registerAllocate,
 dotAllocationGraph,
 ) where

import Data.Maybe
import Data.List
import Data.Function
import Data.Ord
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Dot
import System.Process
import Control.Monad.State

import Debug.Trace

import Language
import qualified X86 as X86

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

registerAllocate asm = map r asm where
 colors = graphColoring . livenessGraph . livenessAnalysis 0 $ asm
 priority = map (snd . head) . sortBy (flip (comparing length)) . groupBy ((==)`on`snd) . labNodes $ colors
 allocation = zip priority ([X86.R X86.EBX, X86.R X86.ECX, X86.R X86.ESI, X86.R X86.EDI] ++ map ((X86.DV) . ("ev"++) . show) [0..])
 r (AAdd x y) = X86.Add (i x) (i y)
 r (ASub x y) = X86.Sub (i x) (i y)
 r (AMul x) = X86.Mul (i x)
 r (AMov x y) = X86.Mov (i x) (i y)
 i (Register ()) = X86.R X86.EAX
 i (Deref ()) = X86.DR X86.EAX
 i (Cell c) = let y = lookup c . labNodes $ colors in
              let x = lookup (fromJust y) $ allocation
               in fromJust x
 i (Num n) = X86.I n

dotAllocationGraph asm = do
    let dot = showDot  . fglToDot . graphColoring . livenessGraph . livenessAnalysis 0 $ asm
    writeFile "file.dot" dot
    system("dot -Tpng -ofile.png file.dot")

