module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.Process (callProcess)
import System.Directory (createDirectoryIfMissing)

import Parser (parser, parserInfo)
import CodeGenerator (codeGen, codeGenInfo)

gxxFlags :: [String]
gxxFlags =
    [ "-std=c++20"
    , "-O3"
    , "-march=native"
    , "-flto"
    , "-DNDEBUG"
    , "-pipe"
    , "-ffunction-sections"
    , "-fdata-sections"
    , "-Wl,--gc-sections"
    , "-Wl,-O1"
    , "-Wl,--as-needed"
    , "-s"
    ]

main :: IO ()
main = do
    putStrLn ("Parser: " ++ parserInfo)
    putStrLn ("Code Generator: " ++ codeGenInfo)
    args <- getArgs
    src  <- getContents
    outFile <- case args of
        ["-o", fname] -> pure fname
        _             -> die "Uso: compiler -o <ejecutable-salida> < input.src"
    let buildDir = "compiler-build/"
    createDirectoryIfMissing True buildDir
    case parser src of
        Left errMsg -> die errMsg
        Right expr  -> do
            let cppCode = codeGen expr
            let cppFile = buildDir ++ outFile ++ ".cpp"
            let binFile = buildDir ++ outFile
            writeFile cppFile cppCode
            callProcess "g++" (gxxFlags ++ [cppFile, "-o", binFile])
            putStrLn $ "Compilado programa a " ++ binFile
