module Ella where

import Data.Maybe
import Control.Monad.State
--import System.Random

-- http://www.youtube.com/watch?v=PbL9vr4Q2LU

type Names a = State (Int,[Int]) a
freshName :: Names Int
freshName = do
 (i,x:xs) <- get
 put (i+1,xs)
 return x
namesUsed :: Names Int
namesUsed = do (i,_) <- get ; return i
runNames m = mapM_ print . fst . runState m $ (0,[-4,-8..])

type Name = String

data Operator
     = Add | Multiply | Subtract
     | Divide | Modulo
     deriving (Eq, Show)

data Size = Byte | Word | DWord | QWord deriving (Eq)

--data Structure = Structure Name [(Name, Size)]

data Exp
 = EVar Name
 | ENum Int
 | EArithmetic Operator Exp Exp
 | EArrayAccess Name Exp
 | EStructAccess Name Name
 | ECall Name [Exp]
 deriving (Eq)
instance Show Exp where
 show (EVar v) = v
 show (ENum i) = show i
 show (EArithmetic op x y) = "(" ++ show x ++ " " ++ o op ++ " " ++ show y ++ ")"
   where o Add = "+"
         o Multiply = "*"
         o Subtract = "-"
         o Divide = "/"
         o Modulo = "%"
 show (EArrayAccess i e) = i ++ "[" ++ show e ++ "]"
 show (ECall i es) = i ++ "(" ++ concatMap (\e -> (show e) ++ ", ") (init es) ++ show (last es) ++ ")"


data Thing
 = TNum Int
 | TMem Int -- local variable or parameter relative ot ebp (base pointer)
 | TReg Name -- e.g. eax
 | TDerefReg Name -- e.g. [ebx]
 deriving (Eq)
instance Show Thing where
  show (TNum i) = show i
  show (TMem l) | l < 0  = "[ebp"  ++ show l ++ "]"
  show (TMem l) | l == 0 = "[ebp]"
  show (TMem l) | l > 0  = "[ebp+" ++ show l ++ "]"
  show (TReg r) = r
  show (TDerefReg r) = "[" ++ r ++ "]"

data Asm
 = AMov Thing Thing
 | AAdd Thing Thing
 | AMul Thing Thing
 | ASub Thing Thing
 | ADiv Thing
 | APush Thing
 | ACall Name
 deriving (Eq)
instance Show Asm where
 show (AMov p1 p2) = "mov dword " ++ show p1 ++ ", " ++ show p2
 show (AAdd p1 p2) = "add dword " ++ show p1 ++ ", " ++ show p2
 show (ASub p1 p2) = "sub dword " ++ show p1 ++ ", " ++ show p2
 show (AMul p1 p2) = "mul dword " ++ show p1 ++ ", " ++ show p2
 show (ADiv p1)    = "div dword " ++ show p1
 show (APush (TReg r)) = "push " ++ r
 show (ACall name) = "call " ++ name

compile :: (Name -> Int) -> Maybe Int -> Exp -> Names [Asm]
compile lookup Nothing (ENum i)  = return [ AMov (TReg "ebx")     (TNum i) ]
compile lookup (Just l) (ENum i) = return [ AMov (TMem l) (TNum i) ]
compile lookup Nothing (EVar v)  = return [ AMov (TReg "ebx")     (TMem (lookup v)) ]
compile lookup (Just l) (EVar v) = error "UNDEFINED"
compile lookup b (EArithmetic op x y) | divisionOperation op = do
  xloc <- freshName
  xc <- compile lookup (Just xloc) x          -- compute x into freshVar
  yc <- compile lookup Nothing y              -- compute y into ebx
  return (xc ++ yc ++ [AMov (TReg "edx") (TNum 0), -- zero out edx
                       AMov (TReg "eax") (TMem xloc),     -- copy x into eax
                       ADiv (TReg "ebx")]          -- perform division
             ++ copyResult op)                     -- copy the result into 
  where divisionOperation Divide = True
        divisionOperation Modulo = True
        divisionOperation _ = False
        copyResult Divide = [ AMov out (TReg "eax") ] --- out happens to be eax here
        copyResult Modulo = [ AMov out (TReg "edx") ]
        out = case b of
                Nothing -> TReg "eax"
                Just l -> TMem l
compile lookup b (EArithmetic op x y) = do
  (xloc,xc) <- case x of
                 EVar v -> do return (lookup v, [])
                 _      -> do xloc <- freshName
                              xc <- compile lookup (Just xloc) x
                              return (xloc,xc)
  yc <- compile lookup Nothing     y
  
  return (  xc
         ++ yc
         ++ [ aop op (TReg "ebx") (TMem xloc) ]
         ++ case b of
              Nothing -> []
              Just l  -> [ AMov (TMem l) (TReg "ebx") ])
  where aop Add = AAdd
        aop Multiply = AMul
        aop Subtract = ASub
compile lookup b (EArrayAccess name index) = do
  add <- compile lookup Nothing (EArithmetic Add (EVar name) index)
  return (add ++ [AMov (TReg "ebx") (TDerefReg "ebx")])
compile lookup' b (EStructAccess name field) =
  return ([AMov (TReg "ebx") (TMem (pointer + offset))] ++ 
          destination) where
    pointer = lookup' name
    fields = fromJust (lookup name structureTable)
    offset = offset' field fields 0 where
      offset' field [] n = error "undefined field in struct"
      offset' field ((i, t):fs) n = if field == i
        then n
        else offset' field fs (n + (fromJust (lookup t sizeTable)))
    destination = case b of
      Just location -> [AMov (TMem location) (TReg "ebx")]
      Nothing       -> []
compile lookup b (ECall name args) = do
 computeArgs <- mapM computeArg args
 return (   concat computeArgs
         ++ [ ACall name ]
         ++ [ AAdd (TReg "esp") (TNum (length args*4)) ]
         ++ case b of
               Nothing -> [ AMov (TReg "ebx") (TReg "eax") ]
               Just l  -> [ AMov (TMem l) (TReg "eax") ])
  where computeArg arg = do c <- compile lookup Nothing arg
                            return (c ++ [ APush (TReg "ebx") ])

lookupTable t k = fromJust . lookup k $ t

basicTable "a" = 8
basicTable "b" = 12
basicTable "c" = 16
basicTable "d" = 20
basicTable "e" = 24

-- example structure table
structureTable = [("a", [("a", DWord),
                          ("b", DWord)]),
                  ("b", [("p", QWord),
                          ("q", DWord),
                          ("r", Word)])]

sizeTable = [(Byte, 1),
             (Word, 2),
             (DWord, 4),
             (QWord, 8)]

optimizeAssembly :: [Asm] -> [Asm]
optimizeAssembly assembly = single assembly where
  single instructions = let
    predicate (AMov x y) | x == y = False
    predicate (AAdd x y) | x == y = False
    predicate (ASub x y) | x == y = False
    predicate (AMul x y) | x == y = False
    predicate _                   = True
    in filter predicate instructions

{-
test = do e <- randomExp
          print e
          runNames $ compile basicTable Nothing e
-}

testCase e = do 
              writeFile "f.asm" (generateAsm e)
              writeFile "g.c" (generateC e)
              putStrLn (generateAsm e)
 where generateC e = "int g(int a, int b, int c, int d, int e) { return " ++ show e ++ "; }"
       generateAsm e = let insts = unlines . map (("  " ++) . show)  $ compiled
                           (compiled,state) = runState (compile basicTable Nothing e) $ (0,[-4,-8..])
                           localSize = 4 * (fst $ state)
                       in unlines ["section .text", "  global f", "f:", "  push ebp"] ++
                          unlines ["  push ebx", "  mov ebp, esp", "  sub esp, " ++ show localSize] ++
                          insts ++
                          unlines ["  mov esp, ebp", "  pop ebx", "  pop ebp", "  ret"]
-- TODO compiler should save ebx
{-
size (EVar _) = 1
size (ENum _) = 1
size (EAdd x y) = size x + size y

randomExp' = do e <- randomExp
                if size e > 2
                   then return e
                   else randomExp'
randomExp = do
    r <- randomRIO (0, 2) :: IO Int
    case r of
        0 -> do
            identifier <- randomRIO ('a', 'e')
            return (EVar [identifier])
        1 -> do
            n <- randomRIO (0, 4096)
            return (ENum n)
        2 -> do
            e1 <- randomExp
            e2 <- randomExp
            return (EAdd e1 e2)

main = testCase
-}
