module BeeParser
 ( beeCalculation
 , beeBlock
 ) where

import BeeSyntax
import ExpressionParser
-- Expression Parser needs to handle unary ops

import Text.ParserCombinators.ReadP

name = many1 (choice (map char ['a'..'z']))
beeName   = do x <- name
               return (BeeVar x)
beeNumber = do n <- many1 (choice (map char ['0'..'9']))
               return (BeeNum (read n :: Int))

beeCalculationLeaf = beeName +++ beeNumber
beeCalculationTree = parseExpression beeCalculationLeaf operators
 where operators = [ (Cmp,"=")
                   , (Add,"+"), (Sub,"-"), (Mul,"*")
                   , (Div,"/"), (Xor,"^"), (And,"&")
                   , (Or,"|"), (Shl,"<<"), (Shr,">>")
                   ] -- may require re-ordering

beeCalculation = (do skipWhitespace
                     calculation <- beeCalculationTree
                     return $ Calculation Nothing calculation) +++
                 (do skipWhitespace
                     x <- name
                     skipWhitespace
                     string ":="
                     skipWhitespace
                     calculation <- beeCalculationTree
                     return $ Calculation (Just x) calculation)

braces p = do skipWhitespace
              char '{'
              r <- p
              skipWhitespace
              char '}'
              return r

optionally p = (do x <- p ; return (Just x)) +++ return Nothing

beeBlock = braces $ do label <- optionally $ do skipWhitespace
                                                label <- name
                                                char ':'
                                                return label
                       code <- many beeCode
                       return $ Block label code

beeCode = beeCalculation +++ beeBlock
