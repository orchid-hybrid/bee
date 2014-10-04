module Regex where

import Control.Monad.Trans
import Control.Monad.Writer
import Data.Monoid

import Fresh
import X86

data Regex
 = Char Char
 | Regex :^ Regex
 | Regex :| Regex
 | Star Regex

data Bytecode
 = CHAR Char
 | LABEL Label
 | SPLIT Label Label
 | JUMP Label

compile :: Regex -> WriterT [Bytecode] (Fresh Label) ()
compile (Char c) = tell [CHAR c]
compile (r1 :^ r2) = do compile r1 ; compile r2
compile (r1 :| r2) = do
 l1 <- lift $ fresh ; l2 <- lift $ fresh ; l3 <- lift $ fresh
 tell [SPLIT l1 l2]
 tell [LABEL l1] ; compile r1 ; tell [JUMP l3]
 tell [LABEL l2] ; compile r2
 tell [LABEL l3]
compile (Star r) = do
 l1 <- lift $ fresh ; l2 <- lift $ fresh ; l3 <- lift $ fresh
 tell [LABEL l1, SPLIT l2 l3]
 tell [LABEL l2] ; compile r ; tell [JUMP l1]
 tell [LABEL l3]

-- rax is the buffer pointer
-- rbx is the end
elab :: Bytecode -> [X86]
elab (CHAR c) = 
 [ Cmp Nothing (R RAX) (R RBX)
 , Jge ".fail"
 , Cmp (Just Byte) (DR RAX) (C c)
 , Jne ".fail"
 , Inc (R RAX)
 ]
elab (LABEL l1) = [Label l1]
elab (SPLIT l1 l2) =
 [ Push (R RAX)
 , Push (V l2)
 , Jmp l1
 ]
elab (JUMP l1) = [Jmp l1]

runCompile regexp = (snd $ runFresh (runWriterT $ compile regexp) labels) >>= (s . elab)
 where labels = map ((".l"++).show) [0..]
