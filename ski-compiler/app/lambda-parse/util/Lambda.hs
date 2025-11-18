module Lambda(
    Id(..),
    Lambda(..),
    showLambda,
) where

import Lexer
import Data.Hashable

-- |Identificadores lambda
data Id = Id String MetaToken
    deriving (Show)
instance Eq Id where
    (==) (Id s1 _) (Id s2 _) = s1 == s2
instance Ord Id where
    compare (Id s1 _) (Id s2 _) = compare s1 s2
instance Hashable Id where
    hashWithSalt salt (Id s _) = hashWithSalt salt s

-- |Expresión lambda
data Lambda
    = LId Id
    | LStr String
    | LApp Lambda Lambda
    | LAbs Id Lambda
    deriving (Show,Eq)

-- |Imprime una expresión formateada
showLambda :: Lambda -> String
showLambda (LId (Id s _)) = s
showLambda (LStr s) = show s
showLambda (LApp l1 l2) = "(" ++ showLambda l1 ++ " " ++ showLambda l2 ++ ")"
showLambda (LAbs (Id s _) b) = "[\\" ++ s ++ " . " ++ showLambda b ++ "]"
