module BeeParserTests where

import BeeSyntax
import BeeParser

import Text.ParserCombinators.ReadP

testReadP p = {-listToMaybe . -}map fst . filter (null .snd) . readP_to_S p

testsArith =
 [ "x"
 , "234"
 , "x+1"
 , "x+3*y"
 , "z*w+h"
 , "z*(w+h)"
 ]

testArith = testReadP beeCalculation

testsBlock =
 [ "{}"
 , "{ x y z }"
 , "{foo: x y z }"
 , "{foo: a {bar: x y} {b c}}"
 ]

testBlock = testReadP beeBlock
