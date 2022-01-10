import Data.Char

-- utility function for handling exceptions
expected x = error $ x ++  " expected"

-- Expression type
data Expression =
    Num Char
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    deriving (Show)

mulOp :: Parser (Expression -> Expression -> Expression)
mulOp = literal '*' >>> (\_ -> Mul)
    <|> literal '/' >>> (\_ -> Div)


addOp :: Parser (Expression -> Expression -> Expression)
addOp = literal '+' >>> (\_ -> Add)
    <|> literal '-' >>> (\_ -> Sub)

-- factor is a single digit
factor :: Parser Expression
factor = digit >>> Num
       <|> literal '(' <-+> expression <+-> literal ')'

-- term is any result of a multiplication operation involving factors
-- so basically all multiplication has to happen before we get to addition/subtraction
term :: Parser Expression
term = factor +> term'
term' e = mulOp <+> term >>> buildOp e +> term'
      <|> result e

-- Parser type takes a string and returns element of any type it parsed out, and the remaining string
type Parser a = String -> Maybe (a, String)

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

char :: Parser Char
char [] = Nothing
char (x:xs) = Just(x, xs)

digit :: Parser Char
digit = char <=> isDigit

space :: Parser Char
space = char <=> isSpace

letter :: Parser Char
letter = char <=> isAlpha

literal :: Char -> Parser Char
literal c = char <=> (==c)

alphanum :: Parser Char
alphanum = digit <|> letter

data Assign = Assign Char Expression
    deriving (Show)

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of
            Nothing -> error "Invalid assignment"
            Just ((a,b), _) -> (a, b)

assign :: Parser (Char, Expression)
assign = letter <+-> literal '=' <+> expression

result :: a -> Parser a
result a cs = Just(a, cs)

buildOp :: Expression -> ((Expression -> Expression -> Expression), Expression) -> Expression
buildOp expressionA (op, expressionB) = op expressionA expressionB

expression :: Parser Expression
expression = factor +> expression'
expression' e = addOp <+> factor >>> buildOp e +> expression' <|> result e
