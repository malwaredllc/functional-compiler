module Main where

import Data.Char
import System.Environment
import Core.Parser
import Core.Grammar.Control
import Core.Generator.Nasm

main :: IO ()
main = getArgs >>= p . head


parse :: String -> Program
parse s = case program s of
            Nothing -> error "Invalid program"
            Just (a, b) -> a

-- parse and show toString output in console
p = putStrLn . show . parse

-- parse and emit assembly
-- e = putStrLn . emit . parse