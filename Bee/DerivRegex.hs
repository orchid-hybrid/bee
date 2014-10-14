{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Maybe
import Data.List
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State

import Data.Int
import Data.List
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Dot

import System.Process

data R c
 = Empty
 | Epsilon
 | Symbol c
 | R c :^ R c
 | Kleene (R c)
 | R c :| R c
 | R c :& R c
 | Not (R c)
 deriving (Eq, Ord, Show)

v Empty = False
v Epsilon = True
v (Symbol _) = False
v (r :^ s) = v r && v s
v (Kleene r) = True
v (r :| s) = v r || v s
v (r :& s) = v r && v s
v (Not r) = not (v r)

v' r = if v r then Epsilon else Empty

d _ Empty = Empty
d _ Epsilon = Empty
d a (Symbol a') | a == a' = Epsilon
d _ (Symbol _) = Empty
d a (r :^ s) = (d a r :^ s) :| (v' r :^ d a s)
d a (Kleene r) = d a r :^ (Kleene r)
d a (r :| s) = d a r :| d a s
d a (r :& s) = d a r :& d a s
d a (Not r) = Not (d a r)

d' [] r = r
d' (a:as) r = d' as (d a r)

match r s = v (d' s r)

----------------------------------------------------

simpl (Kleene (Kleene r)) = Just $ Kleene r
simpl (Not (Not r)) = Just $ r
simpl (Epsilon :| (r :^ Kleene r')) | r == r' = Just $ Kleene r
simpl (Epsilon :| (Kleene r' :^ r)) | r == r' = Just $ Kleene r

simpl (Empty :^ r) = Just $ Empty
simpl (r :^ Empty) = Just $ Empty
simpl (Epsilon :^ r) = Just $ r
simpl (r :^ Epsilon) = Just $ r
simpl (Empty :| r) = Just $ r
simpl (r :| Empty) = Just $ r

simpl (r :| r') | r == r' = Just $ r
simpl (r :& r') | r == r' = Just $ r

simpl ((a :^ b) :^ c) = Just $ a :^ (b :^ c)
simpl ((a :| b) :| c) = Just $ a :| (b :| c)
simpl ((a :& b) :& c) = Just $ a :& (b :& c)

simpl (r :| s) | r > s = Just $ s :| r
simpl (r :& s) | r > s = Just $ s :& r

simpl ((r1 :| r2) :^ r) = Just $ ((r1 :^ r) :| (r2 :^ r))
simpl ((r1 :& r2) :^ r) = Just $ ((r1 :^ r) :& (r2 :^ r))
simpl (r :^ (r1 :| r2)) = Just $ ((r :^ r1) :| (r :^ r2))
simpl (r :^ (r1 :& r2)) = Just $ ((r :^ r1) :& (r :^ r2))

simpl _ = Nothing

maybeFix f x = case f x of
     Just x' -> maybeFix f x'
     Nothing -> x

cong f Empty = Empty
cong f Epsilon = Epsilon
cong f (Symbol c) = Symbol c
cong f (r :^ s) = f (cong f r :^ cong f s)
cong f (Kleene r) = f (Kleene (cong f r))
cong f (r :| s) = f (cong f r :| cong f s)
cong f (r :& s) = f (cong f r :& cong f s)
cong f (Not r) = f (Not (cong f r))

simplify :: Ord c => R c -> R c
simplify = cong (maybeFix simpl)

r1 === r2 = simplify r1 == simplify r2

----------------------------------------------------

type DFA sigma = Gr (Bool, R sigma) sigma

newtype Gensym key val a = Gensym { unGensym :: State ([(key,val)],[key]) a }
 deriving (Functor, Monad)

runGensym m s = runState (unGensym m) ([],s)

gensym val = do (store, key:xs) <- Gensym get ; Gensym (put ((key, val) : store, xs)) ; return key
symgen key = do (store, _) <- Gensym get ; return $ lookup key store
gensymLookup val = do (store, _) <- Gensym get
                      case lookup val (map (\(x,y) -> (y,x)) store) of
                        Nothing -> fmap Right (gensym val)
                        Just key -> return (Left key)

newtype Abstract sigma a = Abstract { unAbstract :: WriterT [(Node, sigma, Node)]
                                                       (StateT [Node] (Gensym Node (Bool, R sigma))) a }
 deriving Monad

runAbstract :: Abstract sigma () -> R sigma -> DFA sigma
runAbstract m r = case runGensym (runStateT (runWriterT (unAbstract (boot >> m))) []) [0..] of -- 66 = (v r,r)
 (((_,val),_),(st,_)) -> makeGraph st val
 where boot = do j <- Abstract (lift . lift $ gensym (v r, r))
                 Abstract (lift $ put [j])

makeGraph :: [(Node, (Bool, R sigma))] -> [(Node, sigma, Node)] -> DFA sigma
makeGraph table g = mkGraph nodes edges
 where nodes = map (\v -> fromJust . find ((==v) . fst) $ table) . nub . sort . concatMap (\(a,_,b) -> [a,b]) $ g
       edges = map (\(v1,e,v2) -> (v1,v2,e)) g

----------------------------------------------------

pickState :: Abstract sigma Node
pickState = do (r:rs) <- Abstract (lift $ get)
               Abstract (lift $ put rs)
               return r

stepStates :: (Ord sigma, Bounded sigma, Enum sigma, Eq sigma) => Abstract sigma ()
stepStates = do i <- pickState
                Just r <- Abstract (lift . lift $ symgen i)
                let edges = [minBound..maxBound]
                mapM_ (stepState i r) edges

stepState :: (Ord sigma, Eq sigma) => Node -> (Bool, R sigma) -> sigma -> Abstract sigma ()
stepState i (b,r) c = do let r' = simplify $ d c r
                         let b' = v r'
                         g <- Abstract (lift . lift $ gensymLookup (b', r'))
                         j <- (case g of
                           Left j -> return j
                           Right j -> do -- this is fresh add to state
                                         states <- Abstract (lift $ get)
                                         Abstract (lift $ put (j:states))
                                         return j)
                         Abstract (tell [(i,c,j)])

abstractInterpreter :: (Bounded sigma, Enum sigma, Eq sigma, Ord sigma) => Abstract sigma ()
abstractInterpreter = do
 states <- Abstract (lift $ get)
 case states of
  [] -> return ()
  _ -> do stepStates ; abstractInterpreter

----------------------------------------------------

go :: (Bounded sigma, Enum sigma, Eq sigma, Ord sigma) => R sigma -> DFA sigma
go = runAbstract abstractInterpreter

display g = do
  writeFile "file.dot" (showDot . fglToDot $ g)
  system("dot -Tpng -ofile.png file.dot")
