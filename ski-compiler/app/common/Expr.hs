module Expr(
    Expr(..),
    showTree,
    optimizeExpr,
) where

-- |AST del cálculo SKI optimizado, basado en
--  https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages-2/
data Expr = SComb
          | KComb
          | IComb
          | BComb
          | CComb
          | S'Comb
          | C'Comb
          | BsComb
          | Str String
          | EApp Expr Expr
          deriving (Show, Eq)

-- |Imprime una expresión optimizada como un árbol binario
showTree :: Expr -> String
showTree = aux (0,[]) ""
    where
        aux :: (Int,[Bool]) -> String -> Expr -> String
        aux (level,bars) prefix expr = case expr of
            SComb -> indent level bars ++ prefix ++ "S\n"
            KComb -> indent level bars ++ prefix ++ "K\n"
            IComb -> indent level bars ++ prefix ++ "I\n"
            BComb -> indent level bars ++ prefix ++ "B\n"
            CComb -> indent level bars ++ prefix ++ "C\n"
            S'Comb -> indent level bars ++ prefix ++ "S'\n"
            C'Comb -> indent level bars ++ prefix ++ "C'\n"
            BsComb -> indent level bars ++ prefix ++ "B*\n"
            Str s -> indent level bars ++ prefix ++ "\"" ++ s ++ "\"\n"
            EApp l r -> indent level bars ++ prefix ++ "App\n"
                        ++ aux (level+1,bars++[True])  "├─›" l
                        ++ aux (level+1,bars++[False]) "└─»" r
        indent _ [] = ""
        indent _ bs = concatMap (\b -> if b then "│  " else "   ") (init bs)

-- |Obtiene la longutid de una expresión
exprLen :: Expr -> Int
exprLen (EApp e1 e2) = exprLen e1 + exprLen e2
exprLen _ = 1

-- |Optimiza una expresión SKI+
optimizeExpr :: Expr -> Expr
optimizeExpr e0 | origLen == minLen   = e0
                | directLen == minLen = directOpt
                | otherwise           = simplOpt
                where origLen = exprLen e0
                      directOpt = optimizeExpr' e0
                      directLen = exprLen directOpt
                      simplOpt = optimizeExpr' (simplExpr e0)
                      simplLen = exprLen simplOpt
                      minLen = min origLen (min directLen simplLen)

-- |Optimiza una expresión SKI+ con el método de Peyton
optimizeExpr' :: Expr -> Expr
optimizeExpr' SComb = SComb
optimizeExpr' KComb = KComb
optimizeExpr' IComb = IComb
optimizeExpr' BComb = BComb
optimizeExpr' CComb = CComb
optimizeExpr' S'Comb = S'Comb
optimizeExpr' C'Comb = C'Comb
optimizeExpr' BsComb = BsComb
optimizeExpr' (Str s) = Str s
optimizeExpr' (EApp e1 e2) =
    let e1' = optimizeExpr' e1
        rhs = optimizeExpr' e2
    in case e1' of
        (EApp SComb lhs) ->
            case (lhs,rhs) of
                -- S (K p) (K q) -> K (p q)
                (EApp KComb p, EApp KComb q) ->
                    EApp KComb (EApp p q)
                -- S (K p) I -> p
                (EApp KComb p, IComb) -> p
                -- S (K p) (B q r) -> B* p q r
                (EApp KComb p, EApp (EApp BComb q) r) ->
                    EApp (EApp (EApp BsComb p) q) r
                -- S (K p) q -> B p q
                (EApp KComb p, q) -> EApp (EApp BComb p) q
                -- S (B p q) (K r) -> C' p q r
                (EApp (EApp BComb p) q, EApp KComb r) ->
                    EApp (EApp (EApp C'Comb p) q) r
                -- S p (K q) -> C p q
                (p, EApp KComb q) -> EApp (EApp CComb p) q
                -- S (B p q) r -> S' p q r
                (EApp (EApp BComb p) q, r) ->
                    EApp (EApp (EApp S'Comb p) q) r
                _ -> EApp e1' rhs
        _ -> EApp e1' rhs

-- |Simplifica la expresión en O(n) a únicamente S, K, I para que funcione
--  mejor el algoritmo de optimización (posiblemente).
simplExpr :: Expr -> Expr
simplExpr SComb = SComb
simplExpr KComb = KComb
simplExpr IComb = IComb
simplExpr (EApp e1 e2) = EApp (simplExpr e1) (simplExpr e2)
simplExpr (Str s) = Str s
simplExpr BComb = EApp (EApp SComb (EApp KComb SComb)) KComb -- B = S (S K) K
simplExpr CComb = -- C = S (S (K (S (K S) K)) S) (K K)
    EApp          -- C = { S ( { S (K (S (K S) K)) } S) } (K K)
        (EApp
          SComb
          (EApp
            (EApp SComb (EApp KComb (EApp (EApp SComb (EApp KComb SComb)) KComb)))
            SComb))
        (EApp KComb KComb)
simplExpr S'Comb = -- S' = Psi = S (S (K S) (S (K K) I)) (K I)
    EApp
    (EApp SComb
      (EApp
        (EApp SComb (EApp KComb SComb))
        (EApp (EApp SComb (EApp KComb KComb)) IComb)))
    (EApp KComb IComb)
simplExpr C'Comb = -- C' = S ( S ( S (K K) (S (K S) (S (K K) I)) ) (K I) ) ( K (K (S (K K) I)) )
  EApp
    (EApp SComb
      (EApp
        (EApp SComb
          (EApp
            (EApp SComb
              (EApp KComb KComb))
            (EApp
              (EApp SComb (EApp KComb SComb))
              (EApp
                (EApp SComb (EApp KComb KComb))
                IComb))))
        (EApp KComb IComb)))
    (EApp KComb (EApp KComb (EApp (EApp SComb (EApp KComb KComb)) IComb)))
simplExpr BsComb = -- B* = S ( S (K K) (S (K S) (S (K K) I)) ) ( K (K (S (K K) I)) )
  EApp
    (EApp SComb
      (EApp
        (EApp SComb (EApp KComb KComb))
        (EApp
          (EApp SComb (EApp KComb SComb))
          (EApp (EApp SComb (EApp KComb KComb)) IComb))))
    (EApp KComb (EApp KComb (EApp (EApp SComb (EApp KComb KComb)) IComb)))
