module BeeSyntax
 ( Name
 , BeeType(..)
 , BeeParameterList
 , BeeFunction(..)
 , BeeCode(..), Condition(..)
 , BeeCalculationLeaf(..)
 , BeeCalculation, BinOp(..), UniOp(..)
 ) where

import ExpressionParser(Tree(..))

type Name = String

data BeeType = U0 | U8 | I8 | U16 | I16 | U32 | I32
 deriving (Eq, Show)

type BeeParameterList = [(Name, BeeType)]

data BeeFunction
 = BeeFunction
    BeeType Name BeeParameterList          -- type declaration
    BeeParameterList                       -- local variables
    [BeeCode]                              -- implementation
 deriving (Eq, Show)

data BeeCode
 = Calculation (Maybe Name) BeeCalculation -- x := 3+x or y&0xF5
 | Block (Maybe Name) [BeeCode]            -- {foo: ... } or {}
 | Loop (Maybe Name) (Maybe Condition)     -- loop; or loop foo;
 | Break (Maybe Name) (Maybe Condition)    -- same but break
 | Return (Maybe Name)
 deriving (Eq, Show)

data Condition
 = Zero
 | Carry
 | Overflow
 | Not Condition
 deriving (Eq, Show)

data BeeCalculationLeaf
 = BeeVar Name
 | BeeNum Int
 deriving (Eq, Show)

type BeeCalculation = Tree BeeCalculationLeaf BinOp

data BinOp
 = Add | Sub | Mul | Div | Xor | And
 | Or  | Shl | Shr | Cmp
 deriving (Eq, Show)

data UniOp
 = Invert
 deriving (Eq, Show)
