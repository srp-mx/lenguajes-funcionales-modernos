module Util(
    retab,
    pullOutError,
    dups,
) where

import Data.List(intercalate)
import Data.Hashable(Hashable)

import qualified Data.HashMap.Strict as HM

-- |Agrega un nivel de indentación a la cadena
retab :: String -> String
retab s = "    " ++ aux [] s
    where aux acc [] = reverse acc
          aux acc ('\n':xs) = aux ("    \n" ++ acc) xs
          aux acc (x:xs) = aux (x:acc) xs

-- |Extrae errores de una lista a un único error
pullOutError :: [Either String a] -> Either String [a]
pullOutError = aux [] []
    where aux [] accr [] = Right $ reverse accr
          aux accl _ [] = Left (intercalate "\n" $ reverse accl)
          aux accl accr (Left m:xs) = aux (m:accl) accr xs
          aux accl accr (Right x:xs) = aux accl (x:accr) xs

-- |Determina los elementos duplicados
dups :: (Eq a, Hashable a) => [a] -> [a]
dups xs = HM.keys $ HM.filter (> (1::Int)) counts
    where counts = HM.fromListWith (+) [(x,1) | x <- xs]
