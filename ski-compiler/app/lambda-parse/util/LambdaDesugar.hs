-- NOTE(srp): Traducción de cálculo lambda a cálculo SKI en tiempo y espacio
--            O(n), donde n es el número de nodos en el AST del cálculo lambda
--            y la constante oculta es a lo más aproximadamente 12.
--            El algoritmo fue tomado de Oleg Kiselyov:
--              - https://okmij.org/ftp/tagless-final/ski.pdf
--            Y el código está basado en
--              - https://okmij.org/ftp/tagless-final/skconv.ml
--            El cual acompaña al paper anterior

module LambdaDesugar(
    lambdaToExpr,
) where

import Lexer
import Expr
import Lambda

import qualified Data.HashMap.Lazy as HM

-- |Gramática lambda extendida con índices de Brujin
data Lam
    = VarZero Id     -- de-Brujin 0
    | VarSucc Lam Id -- de-Brujin n+1
    | LamAbs Lam
    | LamApp Lam Lam
    | LamStr String
    deriving (Show)

-- |Construye un índice de brujin de un identificador a partir de un Int
mkDeBrujinIdx :: Id -> Int -> Lam
mkDeBrujinIdx x 0 = VarZero x
mkDeBrujinIdx x n = VarSucc (mkDeBrujinIdx x (n-1)) x

-- |Transforma una expresión lambda en una con índices de Brujin
toDeBrujin :: Lambda -> Either String Lam
toDeBrujin = aux HM.empty 0
    where aux :: HM.HashMap Id Int -> Int -> Lambda -> Either String Lam
          aux env depth (LId x) = case HM.lookup x env of
                Nothing -> Left $ "[ERROR (Parser)]: Variable libre" ++ idInfo x
                Just bindAt -> Right $ mkDeBrujinIdx x (depth - bindAt - 1)
          aux _ _ (LStr s) = Right $ LamStr s
          aux env depth (LApp e1 e2) = do
                r1 <- aux env depth e1
                r2 <- aux env depth e2
                Right $ LamApp r1 r2
          aux env depth (LAbs x body) =
                let env' = HM.insert x depth env
                in do
                    r <- aux env' (depth+1) body
                    Right $ LamAbs r

-- |Igualdad en Lam
instance Eq Lam where
    (==) (VarZero _) (VarZero _) = True
    (==) (VarSucc l1 _) (VarSucc l2 _) = l1 == l2
    (==) (LamAbs l1) (LamAbs l2) = l1 == l2
    (==) (LamApp l1 l2) (LamApp l1' l2') = (l1 == l1') && (l2 == l2')
    (==) (LamStr s1) (LamStr s2) = s1 == s2
    (==) _ _ = False

-- |Operador que sirve como atajo para EApp, imitando el código en OCaml ($!)
infixl 9 $!!
($!!) :: Expr -> Expr -> Expr
($!!) = EApp

-- |Ski por lotes para obtener el algoritmo lineal, con combinadores n-arios.
--  Corresponde con BulkSki en OCaml.
data Bulk
    = Core Expr
    | Bn Int
    | Cn Int
    | Sn Int
    | UApp Bulk Bulk -- aplicación "universal" de lotes en lotes
    deriving (Eq, Show)

-- |Función auxiliar de inyección de Expr->Bulk
uclose :: Expr -> Bulk
uclose = Core

-- |Función auxiliar de proyección de Bulk->Open (Expr)
uopen :: Bulk -> Expr
uopen (Core d) = d
uopen (Bn n)
    | n <= 0 = IComb
    | n == 1 = BComb
    | otherwise = foldl EApp BComb (replicate (n-1) BComb)
uopen (Cn n)
    | n <= 0 = IComb
    | n == 1 = CComb
    | otherwise = foldl EApp CComb (replicate (n-1) CComb)
uopen (Sn n)
    | n <= 0 = IComb
    | n == 1 = SComb
    | otherwise = foldl EApp SComb (replicate (n-1) SComb)
uopen (UApp x y) = uopen x $!! uopen y

-- |Operador que sirve como atajo para UApp, imitando el código en OCaml
infixl 9 $?
($?) :: Bulk -> Bulk -> Bulk
($?) = UApp

-- |Tipo para términos Ski en lote (Bulk) "abiertos" (aún requieren argumentos
--  para traducirse a Ski) o "cerrados" (ya se tradujeron a Ski) en la
--  conversión. Corresponde con LinearConv.C y LinearConv.N en OCaml.
data Open
    = Closed Expr
    | Need Bulk Int
    deriving (Eq, Show)

-- |Operador para aplicación de Open, imitando el código en OCaml
infixl 9 $$
($$) :: Open -> Open -> Open
($$) (Closed d1) (Closed d2) = Closed (d1 $!! d2)
($$) (Closed d1) (Need d2 l) = Need (Bn l $? uclose d1 $? d2) l
($$) (Need d1 l) (Closed d2) = Need (Cn l $? d1 $? uclose d2) l
($$) (Need d1 l1) (Need d2 l2)
    | l1 == l2 = Need (Sn l1 $? d1 $? d2) l1
    | l1 < l2 = Need (Bn (l2-l1) $? (Sn l1 $? d1) $? d2) l2
    | otherwise = Need (Cn (l1-l2) $? (Bn (l1-l2) $? (Sn l2 $? d1)) $? d2) l1

-- |uI en el código original
bulkI :: Bulk
bulkI = uclose IComb

-- |VarZero en Open, necesita 1 arg
zOpen :: Open
zOpen = Need bulkI 1

-- |VarSucc en Open, sólo debería ser llamado en valores abiertos
sOpen :: Open -> Open
sOpen e = case (Closed KComb) $$ e of
    Need d l -> Need d (l+1)
    Closed _ -> error "!!!sOpen produjo término cerrado inesperado"

-- |Abstracción lambda en Open
lamOpen :: Open -> Open
lamOpen (Closed k) = Closed (KComb $!! k)
lamOpen (Need d 1) = Closed (uopen d) -- uopen es O(l) pero aquí l=1
lamOpen (Need d l) = Need d (l-1)

-- |Observamos un término cerrado para transformar un Open terminado en Ski
observe :: Open -> Expr
observe (Closed d) = d
observe _ = error "!!!debería ser imposible observar un término abierto"

-- |Convierte Lam en un Open (que debería estar cerrado al terminar)
--  Como cada operación es O(1), es evidente que esto es O(n) tanto en espacio
--  como en tiempo
conv :: Lam -> Open
conv (VarZero _) = zOpen
conv (VarSucc e _) = sOpen (conv e)
conv (LamAbs e) = lamOpen (conv e)
conv (LamApp e1 e2) = conv e1 $$ conv e2
conv (LamStr s) = Closed (Str s)

-- |Transforma una expresión lambda extendida en una ski extendida
lamToExpr :: Lam -> Expr
lamToExpr = observe . conv

-- |Traduce un AST expresión lambda en un AST expresión SKI
lambdaToExpr :: Lambda -> Either String Expr
lambdaToExpr l = do
    l' <- toDeBrujin l
    Right $ lamToExpr l'

-- |Muestra información de un identificador
idInfo :: Id -> String
idInfo (Id x m) = " [leído '" ++ x ++ "' en: " ++ pos ++ "]"
    where l = "L:" ++ show (mtkLin m)
          c = "C:" ++ show (mtkCol m)
          i = "Char#" ++ show (mtkIdx m)
          b = "Byte#" ++ show (mtkByte m)
          pos = l ++ " " ++ c ++ ", i.e. " ++ i ++ " " ++ b
