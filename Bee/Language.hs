module Language (
 Op(..), E(..), eval,
 Cell, It(..), A(..),
 ) where

import Data.Int

data Op
 = Add | Sub | Mul
 deriving (Eq, Show)

data E
 = N Int32
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
 | Cell Cell
 | Num Int32
 deriving (Eq, Show)

data A reg
 = AAdd (It reg) (It reg)
 | ASub (It reg) (It reg)
 | AMul (It reg)
 
 | AMov (It reg) (It reg)
 deriving (Eq, Show)
