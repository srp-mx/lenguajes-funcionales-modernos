module Parser(
    parser,
    parserShow,
    parserInfo,
) where

import Lexer
import Expr

import qualified Data.HashSet as HS
import Data.Hashable
import Data.List(intercalate,sortBy)

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

-- |Gramática mixta de combinadores y lambda para la traducción
data LambdaSki
    = CombS
    | CombK
    | CombI
    | LSId Id
    | LSStr String
    | LSApp LambdaSki LambdaSki
    | LSAbs Id LambdaSki

-- |Información del parser
parserInfo :: String
parserInfo = "Parser de cálculo lambda modificado en SKI"

-- |Analiza una expresión lambda y la traduce en una expresión SKI
parser :: String -> Either String Expr
parser s = case parseLambda s of
    Left msg -> Left msg
    Right lmb -> lambdaToExpr lmb

-- |Traduce un AST expresión lambda en un AST expresión SKI
lambdaToExpr :: Lambda -> Either String Expr
lambdaToExpr l =
    if HS.null $ freeVariables l HS.empty
    then toSki $ lambdaWithSki l
    else Left ("[ERROR (Parser)]: Variables libres\n"
                ++ retab (intercalate "\n"
                  (map idInfo
                    (sortBy (\(Id _ m1) (Id _ m2) -> compare (mtkIdx m1) (mtkIdx m2))
                      (HS.toList (freeVariables l HS.empty))))))

-- |Obtiene las variables libres de una expresión lambda
freeVariables :: Lambda -> HS.HashSet Id -> HS.HashSet Id
freeVariables (LId x) xs = if x `HS.member` xs then HS.empty else HS.singleton x
freeVariables (LStr _) _ = HS.empty
freeVariables (LApp l1 l2) xs = HS.union (freeVariables l1 xs) (freeVariables l2 xs)
freeVariables (LAbs x l) xs = freeVariables l (HS.insert x xs)

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

-- |Análisis sintáctico de una expresión lambda
parseLambda :: String -> Either String Lambda
parseLambda s = do
    ts <- lexer s
    parseToks ts

-- |Análisis sintáctico sobre los lexemas de una expresión lambda
parseToks :: [Token] -> Either String Lambda
parseToks [] = Left "[ERROR (Parser)]: Programa vacío"
parseToks ts@(t:_) = case parsePrimary ts of
    Left m -> Left $ "[ERROR (Parser)]: Error debajo" ++ tokInfo t
                     ++ "\n" ++ retab m
    Right (l, []) -> Right l
    Right (_, t':_) -> Left $ "[ERROR (Parser)]: Se leyó un programa pero sobró \
                             \contenido" ++ tokInfo t'

-- |Analiza una expresión lambda y la consume
parsePrimary :: [Token] -> Either String (Lambda, [Token])
parsePrimary [] = Left "[ERROR (Parser)]: Se esperaba una expresión"
parsePrimary (Token _ (String_tk s):ts) = Right (parseStr s, ts)
parsePrimary (Token m (Id_tk s):ts) = Right (LId (Id s m), ts)
parsePrimary (Token _ (Par_op_tk _):ts) = parseApp ts
parsePrimary (Token _ (Bra_op_tk _):ts) = parseAbs ts
parsePrimary (t:_) = Left $ "[ERROR (Parser)]: Token inesperado" ++ tokInfo t

-- |Analiza una aplicación lambda, asumiendo que ya se consumió '(', y consume
parseApp :: [Token] -> Either String (Lambda, [Token])
parseApp ts = do
    (l1, ts') <- parsePrimary ts
    (l2, ts'') <- parsePrimary ts'
    expectToken ts'' (Par_cl_tk ")") (LApp l1 l2)

-- |Analiza una abstracción lambda, asumiendo que ya se consumió '[', y consume
parseAbs :: [Token] -> Either String (Lambda, [Token])
parseAbs ts = do
    (x, ts') <- parseId ts
    (body, ts'') <- parsePrimary ts'
    expectToken ts'' (Bra_cl_tk "]") (LAbs x body)

-- |Analiza un identificador y lo consume
parseId :: [Token] -> Either String (Id, [Token])
parseId [] = Left "[ERROR (Parser)]: Se esperaba un identificador y no se \
                   \encontró nada"
parseId (Token m (Id_tk s):ts) = Right (Id s m, ts)
parseId (t:_) = Left $ "[ERROR (Parser)]: Se esperaba un identificador y se \
                       \encontró un token inesperado" ++ tokInfo t

-- |Espera un token en particular y lo consume
expectToken :: [Token] -> LexCat -> Lambda -> Either String (Lambda, [Token])
expectToken [] ex _ = Left $ "[ERROR (Parser)]: Se esperaba " ++ show ex
                             ++ ", pero se encontró vacío."
expectToken (t:ts) ex l
    | tkCat t == ex = Right (l, ts)
    | otherwise = Left $ "[ERROR (Parser)]: Se esperaba " ++ show ex
                         ++ ", pero se encontró un token inesperado" ++ tokInfo t

-- |Extrae la cadena del código fuente y aplica escapes y demás para que sea
--  la cadena que representa el código fuente.
parseStr :: String -> Lambda
parseStr = LStr . aux []
    where aux acc [] = reverse acc
          aux acc ('\\':'\\':xs) = aux ('\\':acc) xs
          aux acc ('\\':'"':xs) = aux ('"':acc) xs
          aux acc (x:xs) = aux (x:acc) xs

-- |Agrega un nivel de indentación a la cadena
retab :: String -> String
retab s = "    " ++ aux [] s
    where aux acc [] = reverse acc
          aux acc ('\n':xs) = aux ("    \n" ++ acc) xs
          aux acc (x:xs) = aux (x:acc) xs

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
lcStr (Par_op_tk s) = s
lcStr (Par_cl_tk s) = s
lcStr (Bra_op_tk s) = s
lcStr (Bra_cl_tk s) = s
lcStr (String_tk s) = s
lcStr (White_tk s) = s
lcStr (Comnt_tk s) = s
lcStr (Mcomt_tk s) = s
lcStr (Id_tk s) = s

-- |Hace lo que el parser pero lo muestra en formato legible para un humano
parserShow :: String -> String
parserShow s =
    let aux (Left errMsg) = "ERROR\n\n" ++ errMsg
        aux (Right expr) = showTree expr
    in aux $ parser s
