module BeeParserParsec where

import BeeSyntax
import ExpressionParser hiding (parseExpression)

import Text.ParserCombinators.Parsec

operators = [ (Cmp,"=")
            , (Add,"+"), (Sub,"-"), (Mul,"*")
            , (Div,"/"), (Xor,"^"), (And,"&")
            , (Or,"|"), (Shl,"<<"), (Shr,">>")
            ]

spaces1 = many1 space
newline1 = many1 newline
{-lineSep = do
    spaces
    newline
    spaces-}
parens = between (char '(' >> spaces) (char ')' >> spaces)
brackets = between (char '{' >> spaces) (char '}' >> spaces)

maybeP p q = do
    p' <- try p
    return (Just p')
    <|> do
    q
    return Nothing
succeed = return ()    -- this combinator always succeeds

parseIdentifier :: Parser Name
parseIdentifier = many1 alphaNum

-- this can be SLIGHTLY MORE EFFICIENT if we left-factor it
parseType :: Parser BeeType
parseType = choice [
    do try (string "u0");  return U0,
    do try (string "u8");  return U8,
    do try (string "i8");  return I8,
    do try (string "u16"); return U16,
    do try (string "i16"); return I16,
    do try (string "u32"); return U32,
    do try (string "i32"); return I32]

parseParameter :: Parser (Name, BeeType)
parseParameter = do
    parameterType <- parseType
    spaces1
    identifier <- parseIdentifier
    return (identifier, parameterType)

parseParameterList :: Parser BeeParameterList
parseParameterList = parseParameter `sepBy` (do spaces; char ','; spaces)

parseCondition :: Parser Condition
parseCondition = parseCondition' <|> do
    string "not"
    spaces1
    condition <- parseCondition'
    return (Not condition) where
        parseCondition' = choice [
            do string "zero";     return Zero,
            do string "carry";    return Carry,
            do string "overflow"; return Overflow]

parseBreak :: Parser BeeCode
parseBreak = do
    string "break"
    spaces1
    identifier <- maybeP parseIdentifier (char '.')
    spaces1
    condition <- maybeP parseCondition succeed
    return (Break identifier condition)

parseReturn :: Parser BeeCode
parseReturn = do
    string "return"
    spaces1
    identifier <- maybeP parseIdentifier succeed
    return (Return identifier)

parseLoop = do
    string "loop"
    spaces1
    identifier <- maybeP parseIdentifier (char '.')
    spaces1
    condition <- maybeP parseCondition succeed
    return (Loop identifier condition)

parseBlock = brackets (do
    identifier <- maybeP (do
        identifier <- parseIdentifier
        char ':'
        return identifier) succeed
    statements <- many1 parseStatement
    return (Block identifier statements))

parseVariable = do
    identifier <- parseIdentifier
    return (BeeVar identifier)
parseNumber = do
    n <- many1 (oneOf ['0'..'9'])
    return (BeeNum (read n :: Int))

parseLeaf = parseNumber <|> parseVariable
parseTree = parseExpression parseLeaf
parseExpression leafP = foldr (\(op, name) parser ->
    let this = (do
        a <- try parser <|> parens parseTree
        spaces
        (do
            string name
            spaces
            b <- this
            spaces
            return (Branch op a b)) <|> return a) in this)
    (do l <- leafP
        return (Leaf l))
    operators

parseCalculation = do
    identifier <- maybeP (do
        identifier <- parseIdentifier
        spaces
        string ":="
        return identifier) succeed
    spaces
    expression <- parseTree
    return (Calculation identifier expression)

parseFunction :: Parser BeeFunction
parseFunction = do
    functionType <- parseType
    parameters <- parens parseParameterList
    spaces
    identifier <- parseIdentifier
    spaces
    brackets (do
        declarations <- parseParameter `sepBy` newline1
        statements <- parseStatement `sepBy1` newline1
        spaces
        return (BeeFunction functionType identifier parameters declarations statements))

parseStatement :: Parser BeeCode
parseStatement =   parseCalculation
               <|> parseBlock
               <|> parseLoop
               <|> parseBreak
               <|> parseReturn
