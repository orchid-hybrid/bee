module Ella
 (
 ) where

-- http://www.youtube.com/watch?v=PbL9vr4Q2LU

import Data.Maybe
import Control.Monad.State
import System.Random

type Names a = State [Int] a
freshName :: Names Int
freshName = do
 (x:xs) <- get
 put xs
 return x
pushName n = do
 xs <- get
 put (n:xs)
runNames m = mapM_ print . fst . runState m $ [-4,-8..]

type Name = String

data Exp
 = EVar Name
 | ENum Int
 | EAdd Exp Exp
 deriving (Eq)
instance Show Exp where
 show (EVar v) = v
 show (ENum i) = show i
 show (EAdd x y) = "(" ++ show x ++ " + " ++ show y ++ ")"

data Thing
 = TNum Int
 | TMem Int
 | TReg
 deriving (Eq)
instance Show Thing where
  show (TNum i) = show i
  show (TMem l) | l < 0  = "[ebp"  ++ show l ++ "]"
  show (TMem l) | l == 0 = "[ebp]"
  show (TMem l) | l > 0  = "[ebp+" ++ show l ++ "]"
  show (TReg) = "eax"

data Asm
 = AMov Thing Thing
 | AAdd Thing Thing
 deriving (Eq)
instance Show Asm where
 show (AMov p1 p2) = "mov dword " ++ show p1 ++ ", " ++ show p2
 show (AAdd p1 p2) = "add dword " ++ show p1 ++ ", " ++ show p2

compile :: (Name -> Int) -> Maybe Int -> Exp -> Names [Asm]
compile lookup Nothing (ENum i)  = return [ AMov TReg     (TNum i) ]
compile lookup (Just l) (ENum i) = return [ AMov (TMem l) (TNum i) ]
compile lookup Nothing (EVar v)  = return [ AMov TReg     (TMem (lookup v)) ]
compile lookup (Just l) (EVar v) = error "UNDEFINED"
compile lookup b (EAdd x y) = do
  (xloc,xc) <- case x of
                 EVar v -> do return (lookup v, [])
                 _      -> do xloc <- freshName
                              xc <- compile lookup (Just xloc) x
                              return (xloc,xc)
  yc <- compile lookup Nothing     y
  return (   xc
          ++ yc
          ++ [ AAdd TReg (TMem xloc) ]
          ++ case b of
               Nothing -> []
               Just l  -> [ AMov (TMem l) TReg ])

lookupTable t k = fromJust . lookup k $ t

basicTable "a" = 8
basicTable "b" = 12
basicTable "c" = 16
basicTable "d" = 20
basicTable "e" = 24

test = do e <- randomExp
          print e
          runNames $ compile basicTable Nothing e

testCase = do e <- randomExp
              putStrLn $ generateC e
              putStrLn $ generateAsm e
 where generateC e = "int g(int a, int b, int c, int d, int e) { return " ++ show e ++ "; }"
       generateAsm e = let insts = unlines . map (("  " ++) . show) . fst .
                                   runState (compile basicTable Nothing e) $ [-4,-8..]
                       in unlines ["section .text", "  global f", "f:"] ++
                          insts ++ "  ret\n"

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

