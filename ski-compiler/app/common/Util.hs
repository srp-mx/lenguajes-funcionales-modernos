module Util(
    retab
) where

-- |Agrega un nivel de indentación a la cadena
retab :: String -> String
retab s = "    " ++ aux [] s
    where aux acc [] = reverse acc
          aux acc ('\n':xs) = aux ("    \n" ++ acc) xs
          aux acc (x:xs) = aux (x:acc) xs
