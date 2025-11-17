module Expr(
    Expr(..),
    showTree,
) where

-- |AST de una expresión del cálculo SKI
data Expr = SComb
          | KComb
          | IComb
          | Str String
          | EApp Expr Expr
          deriving (Show, Eq)

-- |Imprime una expresión como un árbol binario
showTree :: Expr -> String
showTree = aux (0,[]) ""
    where
        aux :: (Int,[Bool]) -> String -> Expr -> String
        aux (level,bars) prefix expr = case expr of
            SComb -> indent level bars ++ prefix ++ "S\n"
            KComb -> indent level bars ++ prefix ++ "K\n"
            IComb -> indent level bars ++ prefix ++ "I\n"
            Str s -> indent level bars ++ prefix ++ "\"" ++ s ++ "\"\n"
            EApp l r -> indent level bars ++ prefix ++ "App\n"
                        ++ aux (level+1,bars++[True])  "├─›" l
                        ++ aux (level+1,bars++[False]) "└─»" r
        indent _ [] = ""
        indent _ bs = concatMap (\b -> if b then "│  " else "   ") (init bs)
