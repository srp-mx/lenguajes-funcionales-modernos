module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.Process (callProcess)

import Parser (parser, parserInfo)
import CodeGenerator (codeGen, codeGenInfo)

main :: IO ()
main = do
    putStrLn ("Parser: " ++ parserInfo)
    putStrLn ("Code Generator: " ++ codeGenInfo)
    args <- getArgs
    src  <- getContents
    outFile <- case args of
        ["-o", fname] -> pure fname
        _             -> die "Uso: compiler -o <ejecutable-salida> < input.src"
    case parser src of
        Left errMsg -> die errMsg
        Right expr  -> do
            let cppCode = codeGen expr
            let cppFile = outFile ++ ".cpp"
            writeFile cppFile cppCode
            callProcess "g++" ["-O3", "-std=c++17", cppFile, "-o", outFile]
            putStrLn $ "Compilado programa a " ++ outFile
