import Data.Char

data Expression =
    Num Char
    | Add Expression Expression
    | Sub Expression Expression
    deriving (Show)


expected x = error $ x ++  " expected"

term x
    | isDigit x = Num x
    | otherwise = expected " Digit"

addOp x
    | x == '+' = Add
    | x == '-' = Sub
    | otherwise = expected "AddOp"

expression (x:[]) = term x
expression (a:b:c:d:ds) = (addOp d) (expression [a,b,c]) (expression ds)
expression (x:y:zs) = (addOp y) (expression [x]) (expression zs)

emit expr = case expr of
    Num x -> [x]
    Add x y -> emit x ++ " + " ++ emit y
    Sub x y -> emit x ++ " - " ++ emit y

emitLn s = "\t" ++ s ++ "\n"

pushEax = emitLn "PUSH eax"
popEbx = emitLn "POP ebx"
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"

emitAsm expr = case expr of
    Num a -> emitLn ("MOV eax, " ++ [a])
    Add a b -> emitAsm a ++ pushEax ++ emitAsm b ++ add
    Sub a b -> emitAsm a ++ pushEax ++ emitAsm b ++ sub

parseAndEmit = emitAsm . expression

