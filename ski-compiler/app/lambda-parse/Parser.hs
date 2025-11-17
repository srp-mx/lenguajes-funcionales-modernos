module Parser(
    parser,
    parserShow,
    parserInfo,
) where

import Expr
import LambdaParser
import LambdaDesugar

-- |Información del parser
parserInfo :: String
parserInfo = "Parser de cálculo lambda modificado en SKI"

-- |Analiza una expresión lambda y la traduce en una expresión SKI
parser :: String -> Either String Expr
parser s = case parseLambda s of
    Left msg -> Left msg
    Right lmb -> lambdaToExpr lmb

-- |Hace lo que el parser pero lo muestra en formato legible para un humano
parserShow :: String -> String
parserShow s =
    let aux (Left errMsg) = "ERROR\n\n" ++ errMsg
        aux (Right expr) = showTree expr
    in aux $ parser s
