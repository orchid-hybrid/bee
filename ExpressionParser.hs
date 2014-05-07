module ExpressionParser
 ( skipWhitespace
 , Tree(..), parseExpression
 ) where

import Data.Maybe
import Text.ParserCombinators.ReadP

skipWhitespace = do many (choice (map char [' ','\n']))
                    return ()

brackets p = do skipWhitespace
                char '('
                r <- p
                skipWhitespace
                char ')'
                return r

data Tree leaf op = Branch op (Tree leaf op) (Tree leaf op) | Leaf leaf
 deriving (Eq, Show)

-- listToMaybe . map fst . filter (null .snd) . readP_to_S 
parseExpression leafP operators = tree where
 leaf = brackets tree
        +++ do skipWhitespace
               s <- leafP
               return (Leaf s)
 tree = foldr (\(op,name) p ->
                let this = p +++ do a <- p +++ brackets tree
                                    skipWhitespace
                                    string name
                                    b <- this
                                    return (Branch op a b)
                 in this)
              (leaf +++ brackets tree)
              operators
