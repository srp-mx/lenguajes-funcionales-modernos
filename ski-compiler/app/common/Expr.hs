module Expr(
    Expr(..),
    Expr'(..),
    showTree,
    showTree',
    optimizeExpr,
) where

-- |AST de una expresión del cálculo SKI
data Expr = SComb
          | KComb
          | IComb
          | Str String
          | EApp Expr Expr
          deriving (Show, Eq)

-- |AST del cálculo SKI optimizado, basado en
--  https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages-2/
data Expr' = SComb'
           | KComb'
           | IComb'
           | BComb'
           | CComb'
           | S'Comb'
           | C'Comb'
           | BsComb'
           | Str' String
           | EApp' Expr' Expr'
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

-- |Imprime una expresión optimizada como un árbol binario
showTree' :: Expr' -> String
showTree' = aux (0,[]) ""
    where
        aux :: (Int,[Bool]) -> String -> Expr' -> String
        aux (level,bars) prefix expr = case expr of
            SComb' -> indent level bars ++ prefix ++ "S\n"
            KComb' -> indent level bars ++ prefix ++ "K\n"
            IComb' -> indent level bars ++ prefix ++ "I\n"
            BComb' -> indent level bars ++ prefix ++ "B\n"
            CComb' -> indent level bars ++ prefix ++ "C\n"
            S'Comb' -> indent level bars ++ prefix ++ "S'\n"
            C'Comb' -> indent level bars ++ prefix ++ "C'\n"
            BsComb' -> indent level bars ++ prefix ++ "B*\n"
            Str' s -> indent level bars ++ prefix ++ "\"" ++ s ++ "\"\n"
            EApp' l r -> indent level bars ++ prefix ++ "App\n"
                        ++ aux (level+1,bars++[True])  "├─›" l
                        ++ aux (level+1,bars++[False]) "└─»" r
        indent _ [] = ""
        indent _ bs = concatMap (\b -> if b then "│  " else "   ") (init bs)

-- |Optimiza una expresión SKI
optimizeExpr :: Expr -> Expr'
optimizeExpr = optimizeExpr' . transExpr'

-- |Traduce literalmente Expr->Expr'
transExpr' :: Expr -> Expr'
transExpr' SComb = SComb'
transExpr' KComb = KComb'
transExpr' IComb = IComb'
transExpr' (Str s) = Str' s
transExpr' (EApp e1 e2) = EApp' (transExpr' e1) (transExpr' e2)

-- |Optimiza Expr'
optimizeExpr' :: Expr' -> Expr'
optimizeExpr' SComb' = SComb'
optimizeExpr' KComb' = KComb'
optimizeExpr' IComb' = IComb'
optimizeExpr' BComb' = BComb'
optimizeExpr' CComb' = CComb'
optimizeExpr' S'Comb' = S'Comb'
optimizeExpr' C'Comb' = C'Comb'
optimizeExpr' BsComb' = BsComb'
optimizeExpr' (Str' s) = Str' s
optimizeExpr' (EApp' e1 e2) =
    let e1' = optimizeExpr' e1
        rhs = optimizeExpr' e2
    in case e1' of -- Tratamos de optimizar expresiones del tipo -> S a b
        (EApp' SComb' lhs) ->
            case (lhs,rhs) of
                -- S (K p) (K q) -> K (p q)
                (EApp' KComb' p, EApp' KComb' q) ->
                    EApp' KComb' (EApp' p q)
                -- S (K p) I -> p
                (EApp' KComb' p, IComb') -> p
                -- S (K p) (B q r) -> B* p q r
                (EApp' KComb' p, EApp' (EApp' BComb' q) r) ->
                    EApp' (EApp' (EApp' BsComb' p) q) r
                -- S (K p) q -> B p q
                (EApp' KComb' p, q) -> EApp' (EApp' BComb' p) q
                -- S (B p q) (K r) -> C' p q r
                (EApp' (EApp' BComb' p) q, EApp' KComb' r) ->
                    EApp' (EApp' (EApp' C'Comb' p) q) r
                -- S p (K q) -> C p q
                (p, EApp' KComb' q) -> EApp' (EApp' CComb' p) q
                -- S (B p q) r -> S' p q r
                (EApp' (EApp' BComb' p) q, r) ->
                    EApp' (EApp' (EApp' S'Comb' p) q) r
                _ -> EApp' e1' rhs
        _ -> EApp' e1' rhs
