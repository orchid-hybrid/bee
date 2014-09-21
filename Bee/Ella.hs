module Ella (
 ella, runElla,
 compile
 ) where

import Control.Monad.Trans
import Control.Monad.Writer
import Data.Monoid
import Data.List
import Data.Int

import Fresh
import Language
import qualified X86 as X86
import Coloring hiding (registerAllocate)
import qualified Coloring

aop Add = AAdd
aop Sub = ASub

ella :: E -> It () -> WriterT [A ()] (Fresh Int) ()
ella (N i) r = tell [AMov r (Num i)]
ella (O op x y) r = do
 a <- lift $ fresh
 ella y (Cell a)
 ella x (Register ())
 case op of
  Add -> tell [AAdd (Register ()) (Cell a)]
  Sub -> tell [ASub (Register ()) (Cell a)]
  Mul -> tell [AMul (Cell a)]
 if r == Register ()
    then return ()
    else tell [AMov r (Register ())]

runElla exp = snd $ runFresh (runWriterT $ ella exp (Register ())) [0..]

--

registerAllocate = map r where
 r (AAdd x y) = X86.Add (i x) (i y)
 r (ASub x y) = X86.Sub (i x) (i y)
 r (AMul x) = X86.Mul (i x)
 r (AMov x y) = X86.Mov (i x) (i y)
 i (Register ()) = X86.R X86.EAX
 i (Deref ()) = X86.DR X86.EAX
 i (Cell c) = X86.DV ("ev" ++ show c)
 i (Num n) = X86.I n

getSpills = nub . sort . concatMap c where
 c (X86.Add x y) = i x ++ i y
 c (X86.Sub x y) = i x ++ i y
 c (X86.Mul x) = i x
 c (X86.Mov x y) = i x ++ i y
 i (X86.V v) = [v]
 i (X86.DV v) = [v]
 i _ = []

compile exp = do
 let ella = runElla $ exp
 let asm = registerAllocate $ ella
 let asm' = Coloring.registerAllocate $ ella
 let spills = getSpills asm
 let spills' = getSpills asm'
 putStrLn $ "extern print_number"
 putStrLn $ ""
 putStrLn $ "section .bss"
 putStrLn $ unlines (map (\c -> "  " ++ c ++ ": resd 1") spills')
 putStrLn $ ""
 putStrLn $ "section .data"
 putStrLn $ "  global asm_main"
 putStrLn $ "asm_main:"
 putStrLn $ "  enter 0,0"
 putStrLn $ "  pusha"
 putStrLn $ ""
 putStrLn $ "  ;; exp: " ++ show exp
 putStrLn $ "  ;; result: " ++ show (eval exp)
 putStrLn $ "  ;; naive spills: " ++ show (length . nub . sort $ spills)
 putStrLn $ "  ;; optimized spills: " ++ show (length . nub . sort $ spills')
 putStrLn $ ""
 putStrLn $ X86.s asm'
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

