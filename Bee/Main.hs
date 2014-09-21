module Main where

import System.Random

import Language
import Ella

-- ./Main > foo.s && make && grep result foo.s && ./foo

data Tree = Leaf | Branch Tree Tree
 deriving (Eq, Show)

randomSplit n = do
 i <- randomRIO (0, n) :: IO Int
 return (i, n-1)

randomTree n = do
 leaf <- randomRIO (0, 1)
 if n <= leaf
    then return Leaf
    else do (x,y) <- randomSplit n
            left <- randomTree x
            right <- randomTree y
            return (Branch left right)

randomOp = do
 o <- randomRIO (0, 6) :: IO Int
 return $ case o of
  0 -> Add
  1 -> Sub
  2 -> Add
  3 -> Sub
  4 -> Mul
  5 -> Mul
  6 -> Mul

randomExp Leaf = do
 n <- randomRIO (-30, 30)
 return (N n)
randomExp (Branch left right) = do
 x <- randomExp left
 y <- randomExp left
 o <- randomOp
 return (O o x y)

main = do test <- randomExp =<< randomTree 18
          compile test
