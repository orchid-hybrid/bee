module BeeParserTests where

import BeeSyntax
import BeeParser

import Text.ParserCombinators.ReadP

testsArith =
 [ "x"
 , "234"
 , "x+1"
 , "x+3*y"
 , "z*w+h"
 , "z*(w+h)"
 ]

testArith = {-listToMaybe . -}map fst . filter (null .snd) . readP_to_S beeCalculation
