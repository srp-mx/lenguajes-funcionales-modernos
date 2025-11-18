module Parser(
    parser,
    parserShow,
    parserInfo,
) where

import Util
import Expr
import Lexer
import Lambda
import LambdaParser
import LambdaDesugar

import Graph

import Data.List(intercalate)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

-- |Información del parser
parserInfo :: String
parserInfo = "Parser de 'subs' en SKI"

-- |El parser a cálculo SKI
parser :: String -> Either String Expr
parser s = do
    ts0 <- extractTuples s
    ts1 <- freeAccounted ts0
    ts2 <- noDups ts1
    ts3 <- noCircle ts2
    ts4 <- subsBack ts3
    l <- getMain ts4
    lambdaToExpr l

-- |Obtiene las asignaciones desde el código fuente
extractTuples :: String -> Either String [(Id, Lambda)]
extractTuples s =
    let mapDef (x,def) = do
            xs <- parseLambdaIdsT x
            xs' <- noDupIds xs
            def' <- parseLambdaT def
            pure (head xs', includeLambdas def' $ tail xs')
    in do
        ts <- lexer s
        defs <- pullOutError $ map (splitTokEq []) (splitTokScol [] [] ts)
        pullOutError $ map mapDef defs

-- |Nos aseguramos de no tener duplicados a la izquierda de una declaración
noDupIds :: [Id] -> Either String [Id]
noDupIds xs = case dups xs of
    [] -> Right xs
    ds -> Left $ "[ERROR (Parser)]: Se encontraron identificadores duplicados \
                  \en el lado izquierdo de una asignación\n"
                  ++ retab (intercalate "\n" (map idInfo ds))

-- |Agregamos lambdas de una lista de identificadores a un cuerpo
includeLambdas :: Lambda -> [Id] -> Lambda
includeLambdas lam xs = aux lam $ reverse xs
    where aux l [] = l
          aux l (v:vs) = aux (LAbs v l) vs

-- |Divide una cadena en dos donde se lee el primer token de definición (=).
--  Da un error si cualquiera de los lados es vacío o no hay tal token.
splitTokEq :: [Token] -> [Token] -> Either String ([Token], [Token])
splitTokEq [] [] = Left "[ERROR (Parser)]: Asignación sin contenido, este caso\
                         \ no debería ser alcanzable"
splitTokEq (t:_) [] = Left $ "[ERROR (Parser)]: Se esperaba una definición tras\
                              \ el token leído" ++ tokInfo t
splitTokEq [] (t@(Token _ (Def_eq_tk _)):_) = Left $ "[ERROR (Parser)]: El lado\
                                                   \ izquierdo de la definición\
                                                   \ es vacío" ++ tokInfo t
splitTokEq _ [t@(Token _ (Def_eq_tk _))] = Left $ "[ERROR (Parser)]: Definición\
                                                    \ vacía" ++ tokInfo t
splitTokEq lhs (Token _ (Def_eq_tk _):ts) = Right (reverse lhs, ts)
splitTokEq lhs (t:ts) = splitTokEq (t:lhs) ts

-- |Divide una cadena por cada separador (;) en subcadenas no vacías
splitTokScol :: [Token] -> [[Token]] -> [Token] -> [[Token]]
splitTokScol [] acc2 [] = map reverse acc2
splitTokScol acc1 acc2 [] = map reverse (acc1:acc2)
splitTokScol [] acc2 (Token _ (Semicl_tk _):ts) = splitTokScol [] acc2 ts
splitTokScol acc1 acc2 (Token _ (Semicl_tk _):ts) = splitTokScol [] (acc1:acc2) ts
splitTokScol acc1 acc2 (t:ts) = splitTokScol (t:acc1) acc2 ts

-- |Obtiene la expresión lambda del punto de entrada del programa
getMain :: [(Id, Lambda)] -> Either String Lambda
getMain [] = Left "[ERROR (Parser)]: No se definió el punto de entrada `main`"
getMain ((Id "main" _, l):_) = Right l
getMain (_:ds) = getMain ds

-- |Se asegura de que no hayan definiciones duplicadas
noDups :: [(Id, Lambda)] -> Either String [(Id, Lambda)]
noDups defs = aux HS.empty defs
    where aux _ [] = Right defs
          aux seen ((x,_):ds)
            | x `HS.member` seen = Left $ "[ERROR (Parser)]: Definición duplicada"
                                        ++ idInfo x
            | otherwise = aux (HS.insert x seen) ds

-- |Se asegura de que las variables libres sean subconjunto de las definiciones
freeAccounted :: [(Id, Lambda)] -> Either String [(Id, Lambda)]
freeAccounted xs =
    let ids = map fst xs
        ls = map snd xs
        hs = HS.fromList ids
        frees = HS.unions $ map (`freeVariables` hs) ls
    in if HS.null frees
       then Right xs
       else Left $ "[ERROR (Parser)]: Variables libres no definidas\n"
                   ++ retab (intercalate "\n"
                        (map idInfo $ HS.toList frees))

-- |Transforma una lista de definiciones de variables en una gráfica dirigida
defsToGraph :: [(Id, Lambda)] -> Graph Id
defsToGraph xs = Graph (map fst xs) (\x -> freeVariables (def x) HS.empty)
    where defMap = HM.fromList xs
          err x = error $ "[ERROR (Parser)]: Variable libre inesperada" ++ idInfo x
          def x = HM.lookupDefault (err x) x defMap

-- |Se asegura de que no hayan definiciones circulares
noCircle :: [(Id, Lambda)] -> Either String [(Id, Lambda)]
noCircle xs =
    let g = defsToGraph xs
        cs = HS.toList $ verticesInCycles g
    in if null cs
       then Right xs
       else Left $ "[ERROR (Parser)]: Se encontraron definiciones circulares\n"
                   ++ retab (intercalate "\n" (map idInfo cs))
                   ++ "\n    Grupos:\n"
                   ++ retab (retab (intercalate "\n"
                        (map
                          (show . map (\(Id ss _) -> ss) . HS.toList)
                          (stronglyConnectedComponents g))))

-- |Se encarga de aplicar sustituciones para que no queden variables libres en
--  orden topológico inverso
subsBack :: [(Id, Lambda)] -> Either String [(Id, Lambda)]
subsBack xs =
    let g = defsToGraph xs
        ordSubs = reverse $ acyclicSort g
        aux :: HM.HashMap Id Lambda -> [Id] -> Either String [(Id, Lambda)]
        aux defMap [] = Right $ HM.toList defMap
        aux defMap (d:ds) =
            case HM.lookup d defMap of
                Just l  -> aux (HM.insert d (subsFree defMap HS.empty l) defMap) ds
                Nothing -> Left $ "[ERROR (Parser)]: Definición faltante"
                                  ++ idInfo d
    in aux (HM.fromList xs) ordSubs

-- |Sustituye variables libres.
--  Asumimos que todas las variables libres a sustituir no tienen variables
--  libres en su definición (que se encuentra en el diccionario defs), lo
--  cual nos garantiza la consistencia semántica. Esto es dado gracias a que
--  las sustituciones las hacemos en el órden acíclico inverso de las
--  definiciones, y a que no hay ciclos en las definiciones. Por lo tanto,
--  no es necesario hacer ninguna conversión alfa.
subsFree :: HM.HashMap Id Lambda -> HS.HashSet Id -> Lambda -> Lambda
subsFree defs bound (LId x)
    | x `HS.member` bound = LId x
    | otherwise = HM.lookupDefault
                    (error $
                        "[ERROR (Parser)]: Identificador libre inesperado"
                        ++ idInfo x)
                    x defs
subsFree _ _ (LStr s) = LStr s
subsFree defs bound (LApp l1 l2) = LApp (subsFree defs bound l1)
                                        (subsFree defs bound l2)
subsFree defs bound (LAbs x body) = LAbs x $ subsFree defs (HS.insert x bound) body

-- |Hace lo que el parser pero lo muestra en formato legible para un humano
parserShow :: String -> String
parserShow s =
    let aux (Left errMsg) = "ERROR\n\n" ++ errMsg
        aux (Right expr) = showTree expr
    in aux $ parser s

-- |Muestra información de un identificador
idInfo :: Id -> String
idInfo (Id x m) = " [leído '" ++ x ++ "' en: " ++ pos ++ "]"
    where l = "L:" ++ show (mtkLin m)
          c = "C:" ++ show (mtkCol m)
          i = "Char#" ++ show (mtkIdx m)
          b = "Byte#" ++ show (mtkByte m)
          pos = l ++ " " ++ c ++ ", i.e. " ++ i ++ " " ++ b
