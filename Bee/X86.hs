module X86 (
 Register(..), X(..), X86(..),
 s
 ) where

import Data.Char
import Data.Int

data Register
 = EAX | EBX | ECX | EDX
 | ESI | EDI | ESP | EBP
 --R8, R9, R10, R11, R12, R13, R14, R15
 deriving (Eq, Show)

data X
 = I Int32
 | R Register
 | V String -- global variable
 | DR Register
 | DV String

data X86
 = Add X X
 | Sub X X
 | Mul X
 
 | Mov X X

red :: Register -> String
red = map toLower . show

op :: X -> String
op (I i) = show i
op (R r) = red r
op (V s) = s
op (DR r) = "[" ++ red r ++ "]"
op (DV s) = "[" ++ s ++ "]"

s' :: X86 -> String
s' (Add x y) = "add   dword " ++ op x ++ "," ++ op y
s' (Sub x y) = "sub   dword " ++ op x ++ "," ++ op y
s' (Mul x) = "mul   dword " ++ op x
s' (Mov x y) = "mov   dword " ++ op x ++ "," ++ op y

s :: [X86] -> String
s = unlines . map ("  "++) . map s'
