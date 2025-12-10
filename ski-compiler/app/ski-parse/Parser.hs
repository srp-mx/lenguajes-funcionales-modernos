module Parser(
    parser,
    parserShow,
    parserInfo,
) where

import Lexer
import Expr

-- |Tipo Ski para el AST del lenguaje ski
data Ski = S
         | K
         | I
         | Strn String
         | App Ski Ski
         deriving (Show, Eq)

-- |Información del parser
parserInfo :: String
parserInfo = "Parser de código SKI"

-- |Lee un programa del lenguaje y da un error o el árbol de sintaxis abstracta.
parser :: String -> Either String Expr
parser s = case lexer s of
    Left msg -> Left msg
    Right ts -> case parseTok ts Nothing 0 of
        Left msg -> Left msg
        Right (expr, _) -> Right $ skiToExpr expr

-- |Transforma un AST Ski en una Expr
skiToExpr :: Ski -> Expr
skiToExpr S = SComb
skiToExpr K = KComb
skiToExpr I = IComb
skiToExpr (Strn s) = Str s
skiToExpr (App s1 s2) = EApp (skiToExpr s1) (skiToExpr s2)

-- |Parser sobre los tokens.
--  Recibe los tokens, lo que se ha formado y la profundidad de parentizado.
--  Devuelve un error o un árbol de sintaxis abstracta y el sufijo por leer.
parseTok :: [Token] -> Maybe Ski -> Int -> Either String (Ski, [Token])
parseTok [] Nothing _ = Left "[ERROR (Parser)]: Programa vacío"
parseTok [] (Just expr) 0 = Right (expr,[])
parseTok [] _ _ = Left "[ERROR (Parser)]: Paréntesis no balanceados"
parseTok (t:ts) Nothing n = case tkCat t of
    Comb_s_tk _ -> parseTok ts (Just S) n
    Comb_k_tk _ -> parseTok ts (Just K) n 
    Comb_i_tk _ -> parseTok ts (Just I) n 
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
    Comb_s_tk _ -> parseTok ts (Just $ App expr' S) n
    Comb_k_tk _ -> parseTok ts (Just $ App expr' K) n
    Comb_i_tk _ -> parseTok ts (Just $ App expr' I) n
    String_tk s -> parseTok ts (Just $ App expr' $ parseStr s) n
    Par_op_tk _ -> case parseTok ts Nothing (n+1) of
                        Left msg -> Left $ "[ERROR (Parser)]: Error dentro de paréntesis"
                                           ++ tokInfo t ++ "\n" ++ retab msg
                        Right (expr, ts') -> parseTok ts' (Just $ App expr' expr) n
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

-- |Extrae la cadena del código fuente y aplica escapes y demás para que sea
--  la cadena que representa el código fuente.
parseStr :: String -> Ski
parseStr = Strn . aux []
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

-- |Agrega un nivel de indentación a la cadena
retab :: String -> String
retab s = "    " ++ aux [] s
    where aux acc [] = reverse acc
          aux acc ('\n':xs) = aux ("    \n" ++ acc) xs
          aux acc (x:xs) = aux (x:acc) xs
