module LambdaDesugar(
    lambdaToExpr,
) where

import Util
import Lexer
import Expr
import Lambda
import LambdaParser

import qualified Data.HashSet as HS
import Data.List(intercalate,sortBy)

-- | TODO: Aplicar transformación lineal de Simon Peyton Jones Lambda->SKI+

-- |Gramática mixta de combinadores y lambda para la traducción
data LambdaSki
    = CombS
    | CombK
    | CombI
    | LSId Id
    | LSStr String
    | LSApp LambdaSki LambdaSki
    | LSAbs Id LambdaSki

-- |Traduce un AST expresión lambda en un AST expresión SKI
lambdaToExpr :: Lambda -> Either String Expr
lambdaToExpr l =
    if HS.null $ freeVariables l HS.empty
    then toSki $ lambdaWithSki l
    else Left ("[ERROR (Parser)]: Variables libres\n"
                ++ retab (showLambda l) ++ "\n"
                ++ retab (intercalate "\n"
                  (map idInfo
                    (sortBy (\(Id _ m1) (Id _ m2) -> compare (mtkIdx m1) (mtkIdx m2))
                      (HS.toList (freeVariables l HS.empty))))))

-- |Convierte una expresión lambda a una expresión "mixta" con únicamente lambda
lambdaWithSki :: Lambda -> LambdaSki
lambdaWithSki (LId x) = LSId x
lambdaWithSki (LStr s) = LSStr s
lambdaWithSki (LApp l1 l2) = LSApp (lambdaWithSki l1) (lambdaWithSki l2)
lambdaWithSki (LAbs x l) = LSAbs x $ lambdaWithSki l

-- |Transforma una expresión mixta en una expresión SKI
toSki :: LambdaSki -> Either String Expr
toSki CombS = Right SComb
toSki CombK = Right KComb
toSki CombI = Right IComb
toSki (LSStr s) = Right (Str s)
toSki (LSApp a b) = do
    ea <- toSki a
    eb <- toSki b
    Right (EApp ea eb)
toSki (LSId x) = Left $ "[ERROR (Parser)]: Variable libre" ++ idInfo x
toSki (LSAbs x body) = do
        sk <- removeFree x body
        toSki sk


-- |Elimina la variable de una abstracción lambda, la cual es libre en su
--  cuerpo, al aplicar combinadores.
removeFree :: Id -> LambdaSki -> Either String LambdaSki
removeFree x body
    | body `dependsOn` x =
        case body of
            LSId y
                | x == y -> Right CombI
                | otherwise -> Right (LSApp CombK (LSId y))
            LSStr s -> Right (LSApp CombK (LSStr s))
            LSApp m n -> do
                em <- removeFree x m
                en <- removeFree x n
                Right (LSApp (LSApp CombS em) en)
            LSAbs y m -> do
                innerAbs <- removeFree y m
                removeFree x innerAbs
            CombS -> Right $ LSApp CombK CombS
            CombK -> Right $ LSApp CombK CombK
            CombI -> Right $ LSApp CombK CombI
    | otherwise = Right $ LSApp CombK body

-- |Determina si una expresión lambda depende de una variable libre
dependsOn :: LambdaSki -> Id -> Bool
dependsOn (LSId y) x = x == y
dependsOn (LSStr _) _ = False
dependsOn (LSApp l1 l2) x = l1 `dependsOn` x || l2 `dependsOn` x
dependsOn (LSAbs y body) x
    | x == y = False
    | otherwise = body `dependsOn` x
dependsOn _ _ = False

-- |Muestra información de un identificador
idInfo :: Id -> String
idInfo (Id x m) = " [leído '" ++ x ++ "' en: " ++ pos ++ "]"
    where l = "L:" ++ show (mtkLin m)
          c = "C:" ++ show (mtkCol m)
          i = "Char#" ++ show (mtkIdx m)
          b = "Byte#" ++ show (mtkByte m)
          pos = l ++ " " ++ c ++ ", i.e. " ++ i ++ " " ++ b
