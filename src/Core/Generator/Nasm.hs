module Core.Generator.Nasm where

import Core.Parser

-- utility functions for generating NASM x86 assembly
emitLn s = "\t" ++ s ++ "\n"
pushEax = emitLn "PUSH eax"
popEax = emitLn "POP eax"
popEbx = emitLn "POP ebx"
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
mul = popEbx ++ emitLn "MUL ebx"
divide = emitLn "MOV ebx, eax" ++ popEax ++ emitLn "MOV edx, 0" ++ emitLn "DIV ebx"

-- text section (code): translates assignment into assembly. loads value in eax into value at label.
emitTextAssign :: Assign -> String
emitTextAssign (Assign a b) = emitText b ++ emitLn ("MOV [" ++ a ++ "], eax")

-- text section (code): translates mathematical expressions into assembly instructions
emitText :: Expression -> String
emitText expr = case expr of
    Num a       -> emitLn ("MOV eax, " ++ (show a))
    Add a b     -> emitText a ++ pushEax ++ emitText b ++ add
    Sub a b     -> emitText a ++ pushEax ++ emitText b ++ sub
    Mul a b     -> emitText a ++ pushEax ++ emitText b ++ mul
    Div a b     -> emitText a ++ pushEax ++ emitText b ++ divide
    Var a       -> emitLn ("MOV eax, [" ++ a ++ "]")

-- data section (variables known at compile time)
emitDataAssign :: Assign -> String
emitDataAssign (Assign a b) = emitData b

emitData :: Expression -> String
emitData expr = case expr of
    Add a b     -> emitData a ++ emitData b
    Sub a b     -> emitData a ++ emitData b
    Mul a b     -> emitData a ++ emitData b
    Div a b     -> emitData a ++ emitData b
    Var a       -> emitLn (a ++ "\tdd\t0")
    otherwise   -> ""

-- bss section (reserved variables with values not known at compile time)
emitBssAssign :: Assign -> String
emitBssAssign (Assign a b) = emitLn (a ++ "\tresd\t1") ++ emitBss b

emitBss :: Expression -> String
emitBss expr = case expr of
    Add a b     -> emitBss a ++ emitBss b
    Sub a b     -> emitBss a ++ emitBss b
    Mul a b     -> emitBss a ++ emitBss b
    Div a b     -> emitBss a ++ emitBss b
    Var a       -> emitLn (a ++ "\tdd\t0")
    otherwise   -> ""


-- Expression -> Assembly
emit :: Expression -> String
emit expr = "section .data\n" ++ emitData expr
        ++ "section .bss\n" ++ emitBss expr
        ++ "section .text\n" ++ emitText expr
