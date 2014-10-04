{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}

import Data.Int
import Data.List
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Dot
import System.Process
import Control.Monad
import Control.Monad.State

import X86

type NFA = Gr () (Maybe Char)

-- (a|b)*x

example1 =
 [ (0,Nothing,1)
 , (0,Nothing,3)
 , (1,Nothing,4)
 , (1,Nothing,6)
 , (4,Just 'a',5)
 , (6,Just 'b',7)
 , (5,Nothing,2)
 , (7,Nothing,2)
 , (2,Nothing,1)
 , (2,Nothing,3)
 , (3,Just 'x',8)
 ]

makeGraph :: [(Node, Maybe Char, Node)] -> NFA
makeGraph g = mkGraph nodes edges
 where nodes = map (\v -> (v,())) . nub . sort . concatMap (\(a,_,b) -> [a,b]) $ g
       edges = map (\(v1,e,v2) -> (v1,v2,e)) g

display g = do
  writeFile "file.dot" (showDot . fglToDot $ g)
  system("dot -Tpng -ofile.png file.dot")

execute :: NFA -> Node -> Node -> String -> [] String
execute g finish node s | finish == node = return s
execute g finish node s = msum (map (\(_,next,path) -> process next path) (out g node))
 where process next Nothing = execute g finish next s
       process next (Just c) = case s of
        [] -> []
        (c':cs) | c == c' -> execute g finish next cs
                | otherwise -> []

-- execute (makeGraph example1) 8 0 "aaax"

data AsmState = AsmState { asmGraph :: NFA, asmFinalNode :: Node, asmStack :: [(Node, String)] }
type Asm a = State AsmState a

push :: (Node,String) -> Asm ()
push e = do s <- get
            let st = asmStack s
            put (s { asmStack = e:st })

pop :: Asm (Maybe (Node, String))
pop = do s <- get
         let st = asmStack s
         case st of
          [] -> return Nothing
          (x:xs) -> do put (s { asmStack = xs })
                       return (Just x)

execute' :: Asm (Maybe String)
execute' = do final <- liftM asmFinalNode get
              top <- pop
              case top of
                Nothing -> return Nothing
                Just (n, s) -> if n == final
                                  then return (Just s)
                                  else do g <- liftM asmGraph get
                                          mapM_ (process s) (out g n)
                                          execute'
 where process s (_,next,Nothing) = push (next, s)
       process (x:xs) (_,next,Just c) | x == c = push (next, xs)
       process _ _ = return ()

runNFA g final start str = evalState execute' (AsmState g final [(start,str)])

-- *Main> runNFA (makeGraph example1) 8 0 "aaaax"
-- Just ""
