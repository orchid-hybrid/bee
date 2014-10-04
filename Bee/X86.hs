module X86 (
 Register(..), X(..), XSize(..), X86(..), Label,
 s
 ) where

import Data.Char
import Data.Int

data Register
 = EAX | EBX | ECX | EDX
 | ESI | EDI | ESP | EBP
 | RAX | RBX | RCX | RDX
 | RSI | RDI | RSP | RBP
 --R8, R9, R10, R11, R12, R13, R14, R15
 deriving (Eq, Show)

data X
 = I Int32
 | C Char
 | R Register
 | V String -- global variable
 | DR Register
 | DV String
 deriving (Eq, Show)

data XSize
 = Byte | QWord | DWord | Word 
 deriving (Eq, Show)

type Label = String

data X86
 = Add X X
 | Sub X X
 | Mul X
 
 | Mov X X
 
 | Cmp (Maybe XSize) X X
 | Label Label
 | Jmp Label | Jne Label | Jge Label
 | Push X | Inc X
 deriving (Eq, Show)

red :: Register -> String
red = map toLower . show

op :: X -> String
op (I i) = show i
op (C c) = show c
op (R r) = red r
op (V s) = s
op (DR r) = "[" ++ red r ++ "]"
op (DV s) = "[" ++ s ++ "]"

xsize Nothing = ""
xsize (Just Byte) = "byte  "
xsize (Just QWord) = "qword  "
xsize (Just DWord) = "dword  "
xsize (Just Word) = "word  "

s' :: X86 -> String
s' (Add x y) = "  add   dword " ++ op x ++ "," ++ op y
s' (Sub x y) = "  sub   dword " ++ op x ++ "," ++ op y
s' (Mul x)   = "  mul   dword " ++ op x
s' (Mov x y) = "  mov   dword " ++ op x ++ "," ++ op y
s' (Cmp s x y) = "  cmp  " ++ xsize s ++ op x ++ "," ++ op y
s' (Label l) = l ++ ":"
s' (Jmp l)   = "  jmp  " ++ l
s' (Jne l)   = "  jne  " ++ l
s' (Jge l)   = "  jge  " ++ l
s' (Push r)  = "  push " ++ op r
s' (Inc r)   = "  inc  " ++ op r

s :: [X86] -> String
s = unlines . map s'
