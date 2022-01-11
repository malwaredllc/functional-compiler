module Main where

import Data.Char
import System.Environment
import Core.Parser
import Core.Generator.Nasm

main :: IO ()
main = getArgs >>= e . head

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of
            Nothing -> error "Invalid assignment"
            Just ((a,b), _) -> (a, b)

-- parse and show toString output in console
p = putStrLn . show . parse

-- parse and emit assembly
e = putStrLn . emit . parse