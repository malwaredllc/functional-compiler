module Core.Parser where

import Data.Char

-- utility function for handling exceptions
expected x = error $ x ++  " expected"


-- Expression type
data Expression =
    Num Integer
    | Var String
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    deriving (Show)

-- Parser type takes a string and returns element of any type it parsed out, and the remaining string
type Parser a = String -> Maybe (a, String)

mulOp :: Parser (Expression -> Expression -> Expression)
mulOp = literal '*' >>> (\_ -> Mul)
    <|> literal '/' >>> (\_ -> Div)


addOp :: Parser (Expression -> Expression -> Expression)
addOp = literal '+' >>> (\_ -> Add)
    <|> literal '-' >>> (\_ -> Sub)


-- factor is a single digit
factor :: Parser Expression
factor = token(literal '(') <-+> expression <+-> token(literal ')')
        <|> number >>> Num
        <|> letters >>> Var

-- term is any result of a multiplication operation involving factors
-- so basically all multiplication has to happen before we get to addition/subtraction
term :: Parser Expression
term = factor +> term'
term' e = mulOp <+> term >>> buildOp e +> term'
      <|> result e

token :: Parser a -> Parser a
token = (<+-> iter space)

-- Given a parser and a predicate return the parser only if it satisfies the predicate.
infix 7 <=>
(<=>) :: Parser a -> (a -> Bool) -> Parser a
(parser <=> predicate) input =
    case parser input of
        Nothing       -> Nothing
        Just(a,rest)  -> if (predicate a) then Just(a,rest) else Nothing

-- Combine two parsers using a 'or' type operation
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(parserA <|> parserB) input =
	case parserA input of
    	Nothing -> parserB input
    	result -> result

-- Combine two parser together pairing their results up in a tuple
infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(parserA <+> parserB) input =
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
        	Just (resultB, cs) -> Just((resultA, resultB), cs)

-- Sequence operator that discards the second result
infixl 6 <+->
(<+-> ) :: Parser a -> Parser b -> Parser a
(parserA <+->  parserB) input =
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
	        Just (_, cs) -> Just(resultA, cs)

-- Sequence operator that discards the first result
infixl 6 <-+>
(<-+> ) :: Parser a -> Parser b -> Parser b
(parserA <-+>  parserB) input =
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
	        Just (resultB, cs) -> Just(resultB, cs)

-- Transform a parser result
infixl 5 >>>
(>>>) :: Parser a -> (a -> b) -> Parser b
(parser >>> transformer) input =
        case parser input of
                Nothing -> Nothing
                Just (resultA, remainder) -> Just ((transformer resultA), remainder)

-- Extract a parsers result
infix 4 +>
(+>) :: Parser a -> (a -> Parser b) -> Parser b
(parser +> function) input =
	case parser input of
		Nothing -> Nothing
		Just (a, cs) -> function a cs

-- iter function to handle more than single letter/digit
iter :: Parser a -> Parser [a]
iter m = m <+> iter m >>> (\(x, y) -> x:y)
        <|> result []

-- define some parser primitives
char :: Parser Char
char [] = Nothing
char (x:xs) = Just(x, xs)

digit :: Parser Char
digit = char <=> isDigit

digits :: Parser String
digits = iter digit

space :: Parser Char
space = char <=> isSpace

letter :: Parser Char
letter = char <=> isAlpha

letters :: Parser String
letters = iter letter

literal :: Char -> Parser Char
literal c = char <=> (==c)

alphanum :: Parser Char
alphanum = digit <|> letter

number :: Parser Integer
number = literal '-' <-+> digits >>> (\n -> -1 * (read n :: Integer))
        <|> digits >>> (\n -> (read n :: Integer))

data Assign = Assign String Expression
    deriving (Show)

assign :: Parser (String, Expression)
assign = token(letters) <+-> token(literal '=') <+> expression

result :: a -> Parser a
result a cs = Just(a, cs)

buildOp :: Expression -> ((Expression -> Expression -> Expression), Expression) -> Expression
buildOp expressionA (op, expressionB) = op expressionA expressionB

expression :: Parser Expression
expression = token(term) +> expression'
expression' e = addOp <+> term >>> buildOp e +> expression' <|> result e
