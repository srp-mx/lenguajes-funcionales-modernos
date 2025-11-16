module Parser(
    Expr(..),
    parser,
    parserShow,
) where

import Lexer

-- |AST de una expresión del cálculo SKI
data Expr = SComb
          | KComb
          | IComb
          | Str String
          | EApp Expr Expr
          deriving (Show, Eq)

-- |Lee un programa del lenguaje y da un error o el árbol de sintaxis abstracta.
parser :: String -> Either String Expr
parser s = case lexer s of
    Left msg -> Left msg
    Right ts -> case parseTok ts Nothing 0 of
        Left msg -> Left msg
        Right (expr, _) -> Right expr

-- |Parser sobre los tokens.
--  Recibe los tokens, lo que se ha formado y la profundidad de parentizado.
--  Devuelve un error o un árbol de sintaxis abstracta y el sufijo por leer.
parseTok :: [Token] -> Maybe Expr -> Int -> Either String (Expr, [Token])
parseTok [] Nothing _ = Left "[ERROR (Parser)]: Programa vacío"
parseTok [] (Just expr) 0 = Right (expr,[])
parseTok [] _ _ = Left "[ERROR (Parser)]: Paréntesis no balanceados"
parseTok (t:ts) Nothing n = case tkCat t of
    Comb_s_tk _ -> parseTok ts (Just SComb) n
    Comb_k_tk _ -> parseTok ts (Just KComb) n 
    Comb_i_tk _ -> parseTok ts (Just IComb) n 
    String_tk s -> parseTok ts (Just $ parseStr s) n
    Par_op_tk _ -> if null ts
                   then Left $ "[ERROR (Parser)]: Se abrió paréntesis sin cerrar"
                               ++ tokInfo t
                   else case parseTok ts Nothing (n+1) of
                        Left msg -> Left $ "[ERROR (Parser)]: Error dentro de paréntesis"
                                           ++ tokInfo t ++ "\n" ++ retab msg
                        Right (expr, ts') -> parseTok ts' (Just expr) n
    Par_cl_tk _ -> Left $ "[ERROR (Parser)]: Paréntesis vacíos no son permitidos"
                          ++ tokInfo t
    _ -> error "!!!No se filtraron bien los tokens no-producibles en el lexer"
parseTok (t:ts) (Just expr') n = case tkCat t of
    Comb_s_tk _ -> parseTok ts (Just $ EApp expr' SComb) n
    Comb_k_tk _ -> parseTok ts (Just $ EApp expr' KComb) n
    Comb_i_tk _ -> parseTok ts (Just $ EApp expr' IComb) n
    String_tk s -> parseTok ts (Just $ EApp expr' $ parseStr s) n
    Par_op_tk _ -> case parseTok ts Nothing (n+1) of
                        Left msg -> Left $ "[ERROR (Parser)]: Error dentro de paréntesis"
                                           ++ tokInfo t ++ "\n" ++ retab msg
                        Right (expr, ts') -> parseTok ts' (Just $ EApp expr' expr) n
    Par_cl_tk _ ->
        if n == 0
        then Left $ "[ERROR (Parser)]: Se cerró un paréntesis que no abrió"
                    ++ tokInfo t
        else Right (expr',ts)
    _ -> error "!!!No se filtraron bien los tokens no-producibles en el lexer"

-- |Muestra información de un token
tokInfo :: Token -> String
tokInfo t = " [leído '" ++ lcStr (tkCat t) ++ "' en: " ++ pos ++ "]"
    where l = "L:" ++ show (mtkLin $ tkMeta t)
          c = "C:" ++ show (mtkCol $ tkMeta t)
          i = "Char#" ++ show (mtkIdx $ tkMeta t)
          b = "Byte#" ++ show (mtkByte $ tkMeta t)
          pos = l ++ " " ++ c ++ ", i.e. " ++ i ++ " " ++ b

-- |Extrae la cadena del lexema
lcStr :: LexCat -> String
lcStr (Comb_s_tk s) = s
lcStr (Comb_k_tk s) = s
lcStr (Comb_i_tk s) = s
lcStr (Par_op_tk s) = s
lcStr (Par_cl_tk s) = s
lcStr (String_tk s) = s
lcStr (White_tk s) = s
lcStr (Comnt_tk s) = s
lcStr (Mcomt_tk s) = s

-- |Extrae la cadena del código fuente y aplica escapes y demás para que sea
--  la cadena que representa el código fuente.
parseStr :: String -> Expr
parseStr = Str . aux []
    where aux acc [] = reverse acc
          aux acc ('\\':'\\':xs) = aux ('\\':acc) xs
          aux acc ('\\':'"':xs) = aux ('"':acc) xs
          aux acc (x:xs) = aux (x:acc) xs

-- |Hace lo que el parser pero lo muestra en formato legible para un humano
parserShow :: String -> String
parserShow s =
    let aux (Left errMsg) = "ERROR\n\n" ++ errMsg
        aux (Right expr) = showTree expr
    in aux $ parser s

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

-- |Agrega un nivel de indentación a la cadena
retab :: String -> String
retab s = "    " ++ aux [] s
    where aux acc [] = reverse acc
          aux acc ('\n':xs) = aux ("    \n" ++ acc) xs
          aux acc (x:xs) = aux (x:acc) xs
