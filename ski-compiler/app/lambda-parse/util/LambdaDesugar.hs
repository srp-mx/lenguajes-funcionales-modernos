module LambdaDesugar(
    lambdaToExpr,
) where

import Expr
import Lambda
import Kiselyov

-- |Traduce un AST expresión lambda en un AST expresión SKI
lambdaToExpr :: Lambda -> Either String Expr
lambdaToExpr = kiselyov
