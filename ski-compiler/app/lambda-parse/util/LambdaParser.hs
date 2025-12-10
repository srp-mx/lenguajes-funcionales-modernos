module LambdaParser(
    parseLambda,
    parseLambdaId,
    parseLambdaIds,
    parseLambdaT,
    parseLambdaIdT,
    parseLambdaIdsT,
    freeVariables,
    tokInfo,
) where

import Util
import Lexer
import Lambda

import qualified Data.HashSet as HS

-- |Obtiene las variables libres de una expresión lambda
freeVariables :: Lambda -> HS.HashSet Id -> HS.HashSet Id
freeVariables (LId x) xs = if x `HS.member` xs then HS.empty else HS.singleton x
freeVariables (LStr _) _ = HS.empty
freeVariables (LApp l1 l2) xs = HS.union (freeVariables l1 xs) (freeVariables l2 xs)
freeVariables (LAbs x l) xs = freeVariables l (HS.insert x xs)

-- |Análisis sintáctico de una expresión lambda
parseLambda :: String -> Either String Lambda
parseLambda s = do
    ts <- lexer s
    parseLambdaT ts

-- |Análisis sintáctico de una expresión lambda sobre tokens
parseLambdaT :: [Token] -> Either String Lambda
parseLambdaT = parseToks

-- |Análisis sintáctico de un identificador
parseLambdaId :: String -> Either String Id
parseLambdaId s = do
    ts <- lexer s
    parseLambdaIdT ts

-- |Análisis sintáctico de un identificador sobre tokens
parseLambdaIdT :: [Token] -> Either String Id
parseLambdaIdT ts = do
    (x,ts') <- parseId ts
    case ts' of
        [] -> Right x
        (t:_) -> Left $ "[ERROR (Parser)]: Sobraron tokens tras el identificador"
                        ++ tokInfo t

-- |Análisis sintáctico de uno o más identificadores
parseLambdaIds :: String -> Either String [Id]
parseLambdaIds s = do
    ts <- lexer s
    parseLambdaIdsT ts

-- |Análisis sintáctico de uno o más identificadores sobre tokens
parseLambdaIdsT :: [Token] -> Either String [Id]
parseLambdaIdsT = aux []
    where aux [] [] = Left "[ERROR (Parser)]: Se esperaba un identificador"
          aux acc [] = Right (reverse acc)
          aux acc ts = do
            (x,ts') <- parseId ts
            aux (x:acc) ts'

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
parseStr = LStr . (aux []) . cleanPad
    where aux acc [] = reverse acc
          aux acc ('\\':'\\':xs) = aux ('\\':acc) xs
          aux acc ('\\':'"':xs) = aux ('"':acc) xs
          aux acc (x:xs) = aux (x:acc) xs

-- |Muestra información de un token
tokInfo :: Token -> String
tokInfo t = " [leído '" ++ lcStr (tkCat t) ++ "' en: " ++ pos ++ "]"
    where l = "L:" ++ show (mtkLin $ tkMeta t)
          c = "C:" ++ show (mtkCol $ tkMeta t)
          i = "Char#" ++ show (mtkIdx $ tkMeta t)
          b = "Byte#" ++ show (mtkByte $ tkMeta t)
          pos = l ++ " " ++ c ++ ", i.e. " ++ i ++ " " ++ b
