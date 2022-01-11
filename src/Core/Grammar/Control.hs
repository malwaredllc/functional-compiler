module Core.Grammar.Control where

import Core.Parser

-- Program data type is a collection of blocks
data Program = Program Block
    deriving (Show)

-- Block type is a list of Statements
newtype Block = Block [Statement]
    deriving (Show)

-- Statement is an assignment
data Statement = Statement Assign
    deriving (Show)

-- statement parser
statement :: Parser Statement
statement = assign >>> Statement

-- program parser
program :: Parser Program
program = block <+-> accept "end" >>> Program

-- block parser
block :: Parser Block
block = iterS statement >>> Block