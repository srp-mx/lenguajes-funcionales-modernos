module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.Process (callProcess)

import Parser (parser)
import CodeGenerator (codeGen)

main :: IO ()
main = do
    args <- getArgs
    src  <- getContents
    outFile <- case args of
        ["-o", fname] -> pure fname
        _             -> die "Uso: ski-compiler -o <ejecutable-salida> < input.ski"
    case parser src of
        Left errMsg -> die errMsg
        Right expr  -> do
            let cppCode = codeGen expr
            let cppFile = outFile ++ ".cpp"
            writeFile cppFile cppCode
            callProcess "g++" ["-O3", "-std=c++17", cppFile, "-o", outFile]
            putStrLn $ "Compilado programa SKI a " ++ outFile
