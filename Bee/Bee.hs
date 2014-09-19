module Bee (
 Op(..), E(..),
 compile
 ) where

import Control.Monad.Trans
import Control.Monad.Writer
import Data.Monoid
import Data.List

import Fresh
import qualified X86 as X86

data Op
 = Add | Sub | Mul
 deriving (Eq, Show)

data E
 = N Int
 | O Op E E
 deriving (Eq, Show)

eop Add = (+)
eop Sub = (-)
eop Mul = (*)

eval (N i) = i
eval (O op x y) = eop op (eval x) (eval y)

--

type Cell = Int
cells = [1..]

data It reg
 = Register reg
 | Deref reg
 | Cell Int
 | Num Int
 deriving (Eq, Show)

data A reg
 = AAdd (It reg) (It reg)
 | ASub (It reg) (It reg)
 | AMul (It reg) (It reg)
 
 | AMov (It reg) (It reg)
 deriving (Eq, Show)

aop Add = AAdd
aop Sub = ASub
aop Mul = AMul

ella :: E -> It () -> WriterT [A ()] (Fresh Int) ()
ella (N i) r = tell [AMov r (Num i)]
ella (O op x y) r = do
 a <- lift $ fresh
 ella y (Cell a)
 ella x (Register ())
 tell [aop op (Register ()) (Cell a)]
 if r == Register ()
    then return ()
    else tell [AMov r (Register ())]

runElla exp = snd $ runFresh (runWriterT $ ella exp (Register ())) [0..]

--

registerAllocate = map r where
 r (AAdd x y) = X86.Add (i x) (i y)
 r (ASub x y) = X86.Sub (i x) (i y)
 r (AMul x y) = X86.Mul (i x) (i y)
 r (AMov x y) = X86.Mov (i x) (i y)
 i (Register ()) = X86.R X86.EAX
 i (Deref ()) = X86.DR X86.EAX
 i (Cell c) = X86.DV ("ev" ++ show c)
 i (Num n) = X86.I n

getSpills = nub . sort . concatMap c where
 c (X86.Add x y) = i x ++ i y
 c (X86.Sub x y) = i x ++ i y
 c (X86.Mul x y) = i x ++ i y
 c (X86.Mov x y) = i x ++ i y
 i (X86.V v) = [v]
 i (X86.DV v) = [v]
 i _ = []

compile exp = do
 let asm = registerAllocate . runElla $ exp
 let spills = getSpills asm
 putStrLn $ "extern print_number"
 putStrLn $ ""
 putStrLn $ "section .bss"
 putStrLn $ unlines (map (\c -> "  " ++ c ++ ": resd 1") spills)
 putStrLn $ ""
 putStrLn $ "section .data"
 putStrLn $ "  global asm_main"
 putStrLn $ "asm_main:"
 putStrLn $ "  enter 0,0"
 putStrLn $ "  pusha"
 putStrLn $ ""
 putStrLn $ "  ;; exp: " ++ show exp
 putStrLn $ "  ;; result: " ++ show (eval exp)
 putStrLn $ "  ;; spills: " ++ show (length . nub . sort $ spills)
 putStrLn $ ""
 putStrLn $ X86.s asm
 putStrLn $ ""
 putStrLn $ "  push ebp"
 putStrLn $ "  mov ebp,esp"
 putStrLn $ "  push eax"
 putStrLn $ "  call print_number"
 putStrLn $ "  add esp,4"
 putStrLn $ "  mov esp,ebp"
 putStrLn $ "  pop ebp"
 putStrLn $ ""
 putStrLn $ "  popa"
 putStrLn $ "  mov eax,0 ; return 0"
 putStrLn $ "  leave"
 putStrLn $ "  ret"

-- main = compile $ (O Add (O Add (N 2) (N 5)) (N 3))

{-

*Bee> compile $ (O Add (O Add (N 2) (N 5)) (N 3))
  ;; result: 10
  ;; spills: 3

  mov   v0,3
  mov   v1,5
  mov   eax,2
  add   eax,v1
  add   eax,v0

-}

