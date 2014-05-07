module ExpressionParser
 ( skipWhitespace
 , Tree(..), parseExpression
 ) where

import Data.Maybe
import Text.ParserCombinators.ReadP

skipWhitespace = do many (choice (map char [' ','\n']))
                    return ()

brackets p = do char '('
                r <- p
                skipWhitespace
                char ')'
                return r

data Tree leaf op = Branch op (Tree leaf op) (Tree leaf op) | Leaf leaf
 deriving (Eq, Show)

-- listToMaybe . map fst . filter (null .snd) . readP_to_S 
parseExpression leafP operators = tree where
 tree = foldr (\(op,name) p ->
                let this = do a <- p +++ brackets tree
                              skipWhitespace
                              (do string name
                                  b <- this
                                  return (Branch op a b))
                                <++ return a
                 in this)
              (do l <- leafP
                  return (Leaf l))
              operators
