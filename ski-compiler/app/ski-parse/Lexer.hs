-- - CÓDIGO GENERADO USANDO - --
-- srp-mx/compiladores-lexer  --
-- -- -- -- -- -- -- -- -- -- --

{-# OPTIONS_GHC -Wall #-}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use newtype instead of data" -}
{- HLINT ignore "Use guards" -}
{- HLINT ignore "Redundant if" -}
{- HLINT ignore "Redundant $" -}

module Lexer (
    lexer,
    lexerShow,
    lcStr,
    LexCat(..),
    MetaToken(..),
    Token(..)
) where

import Data.Char(isSpace, ord)
import Data.List(isPrefixOf,intercalate)
import qualified Data.Text as TXT
import qualified Data.Text.Encoding as ENC
import qualified Data.ByteString as BSTR

-- |Categorías léxicas
data LexCat = Comb_s_tk String
            | Comb_k_tk String
            | Comb_i_tk String
            | Par_op_tk String
            | Par_cl_tk String
            | String_tk String
            | White_tk String
            | Comnt_tk String
            | Mcomt_tk String
   deriving (Show,Eq,Ord)

-- |Aplica la reversa de un lexema
revLexCat :: LexCat -> LexCat
revLexCat (Comb_s_tk s) = Comb_s_tk $ reverse s
revLexCat (Comb_k_tk s) = Comb_k_tk $ reverse s
revLexCat (Comb_i_tk s) = Comb_i_tk $ reverse s
revLexCat (Par_op_tk s) = Par_op_tk $ reverse s
revLexCat (Par_cl_tk s) = Par_cl_tk $ reverse s
revLexCat (String_tk s) = String_tk $ reverse s
revLexCat (White_tk s) = White_tk $ reverse s
revLexCat (Comnt_tk s) = Comnt_tk $ reverse s
revLexCat (Mcomt_tk s) = Mcomt_tk $ reverse s

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

-- |Metadatos de los componentes léxicos
data MetaToken = MetaToken {
   mtkLin :: Int,
   mtkCol :: Int,
   mtkIdx :: Int,
   mtkByte :: Int
} deriving (Show,Eq)

-- |Componente léxico con su categoría léxica y metadatos
data Token = Token {
   tkMeta :: MetaToken,
   tkCat :: LexCat
} deriving (Show,Eq)

-- |Un discriminador toma una cadena y da el prefijo aceptado más largo o error
--  Toma una cadena y da nada o una tupla con lo que queda por leer y el token
--  más grande aceptable por el discriminador
discr :: String -> Maybe (String, LexCat)
discr s = discriminator s 10 [] Nothing

-- |Auxiliar para el discriminador
--  Toma una cadena, la configuración actual y la última configuración aceptada
discriminator :: String -> Int -> String -> Maybe (String, LexCat) -> Maybe (String, LexCat)
discriminator [] 0 accS _ =
    Just ([], White_tk $ reverse accS)
discriminator (x:xs) 0 accS _ =
    discriminator xs (discTrans 0 x) (x:accS) $
        Just (x:xs, White_tk $ accS)
discriminator [] 1 accS _ =
    Just ([], String_tk $ reverse accS)
discriminator (x:xs) 1 accS _ =
    discriminator xs (discTrans 1 x) (x:accS) $
        Just (x:xs, String_tk $ accS)
discriminator [] 2 accS _ =
    Just ([], String_tk $ reverse accS)
discriminator (x:xs) 2 accS _ =
    discriminator xs (discTrans 2 x) (x:accS) $
        Just (x:xs, String_tk $ accS)
discriminator [] 3 accS _ =
    Just ([], Par_op_tk $ reverse accS)
discriminator (x:xs) 3 accS _ =
    discriminator xs (discTrans 3 x) (x:accS) $
        Just (x:xs, Par_op_tk $ accS)
discriminator [] 4 accS _ =
    Just ([], Par_cl_tk $ reverse accS)
discriminator (x:xs) 4 accS _ =
    discriminator xs (discTrans 4 x) (x:accS) $
        Just (x:xs, Par_cl_tk $ accS)
discriminator [] 5 accS _ =
    Just ([], Mcomt_tk $ reverse accS)
discriminator (x:xs) 5 accS _ =
    discriminator xs (discTrans 5 x) (x:accS) $
        Just (x:xs, Mcomt_tk $ accS)
discriminator [] 6 accS _ =
    Just ([], Comnt_tk $ reverse accS)
discriminator (x:xs) 6 accS _ =
    discriminator xs (discTrans 6 x) (x:accS) $
        Just (x:xs, Comnt_tk $ accS)
discriminator [] 7 accS _ =
    Just ([], Comb_s_tk $ reverse accS)
discriminator (x:xs) 7 accS _ =
    discriminator xs (discTrans 7 x) (x:accS) $
        Just (x:xs, Comb_s_tk $ accS)
discriminator [] 8 accS _ =
    Just ([], Comb_k_tk $ reverse accS)
discriminator (x:xs) 8 accS _ =
    discriminator xs (discTrans 8 x) (x:accS) $
        Just (x:xs, Comb_k_tk $ accS)
discriminator [] 9 accS _ =
    Just ([], Comb_i_tk $ reverse accS)
discriminator (x:xs) 9 accS _ =
    discriminator xs (discTrans 9 x) (x:accS) $
        Just (x:xs, Comb_i_tk $ accS)
discriminator (x:xs) 10 accS accC =
    discriminator xs (discTrans 10 x) (x:accS) accC
discriminator (x:xs) 12 accS accC =
    discriminator xs (discTrans 12 x) (x:accS) accC
discriminator (x:xs) 13 accS accC =
    discriminator xs (discTrans 13 x) (x:accS) accC
discriminator (x:xs) 14 accS accC =
    discriminator xs (discTrans 14 x) (x:accS) accC
discriminator (x:xs) 15 accS accC =
    discriminator xs (discTrans 15 x) (x:accS) accC
discriminator (x:xs) 16 accS accC =
    discriminator xs (discTrans 16 x) (x:accS) accC
discriminator _ _ _ Nothing = Nothing
discriminator _ _ _ (Just (rest, tok)) = Just (rest, revLexCat tok)

-- |Auxiliar para usar la función de transición del discriminador
discTrans :: Int -> Char -> Int
discTrans q s = discriminatorTrans q $ ord s

-- |Controla las transiciones de estado del discriminador.
--  Recibe estado y caracter codificado, regresa otro estado.
discriminatorTrans :: Int -> Int -> Int
discriminatorTrans 0 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then if x <= 12
                        then if x <= 6
                          then 11
                          else if x <= 9
                            then if x <= 8
                              then 11
                              else 0
                            else 0
                        else if x <= 18
                          then if x <= 15
                            then if x <= 14
                              then if x <= 13
                                then 0
                                else 11
                              else 11
                            else 11
                          else 11
                      else if x <= 36
                        then if x <= 30
                          then 11
                          else if x <= 33
                            then if x <= 32
                              then if x <= 31
                                then 11
                                else 0
                              else 11
                            else 11
                        else 11
                    else 11
                  else if x <= 144
                    then 11
                    else if x <= 168
                      then if x <= 156
                        then 11
                        else if x <= 162
                          then if x <= 159
                            then 11
                            else if x <= 161
                              then if x <= 160
                                then 0
                                else 11
                              else 11
                          else 11
                      else 11
                else 11
              else 11
            else 11
          else 11
        else if x <= 4609
          then 11
          else if x <= 5377
            then 11
            else if x <= 5761
              then if x <= 5569
                then 11
                else if x <= 5665
                  then 11
                  else if x <= 5713
                    then 11
                    else if x <= 5737
                      then 11
                      else if x <= 5749
                        then 11
                        else if x <= 5755
                          then 11
                          else if x <= 5758
                            then 11
                            else if x <= 5760
                              then if x <= 5759
                                then 11
                                else 0
                              else 11
              else 11
      else if x <= 9218
        then if x <= 7682
          then 11
          else if x <= 8450
            then if x <= 8066
              then 11
              else if x <= 8258
                then if x <= 8162
                  then 11
                  else if x <= 8210
                    then if x <= 8186
                      then 11
                      else if x <= 8198
                        then if x <= 8192
                          then if x <= 8189
                            then 11
                            else if x <= 8191
                              then 11
                              else 0
                          else 0
                        else if x <= 8204
                          then if x <= 8201
                            then 0
                            else if x <= 8203
                              then if x <= 8202
                                then 0
                                else 11
                              else 11
                          else 11
                    else if x <= 8234
                      then 11
                      else if x <= 8246
                        then if x <= 8240
                          then if x <= 8237
                            then 11
                            else if x <= 8239
                              then if x <= 8238
                                then 11
                                else 0
                              else 11
                          else 11
                        else 11
                else if x <= 8354
                  then if x <= 8306
                    then if x <= 8282
                      then 11
                      else if x <= 8294
                        then if x <= 8288
                          then if x <= 8285
                            then 11
                            else if x <= 8287
                              then if x <= 8286
                                then 11
                                else 0
                              else 11
                          else 11
                        else 11
                    else 11
                  else 11
            else 11
        else if x <= 10755
          then 11
          else if x <= 11523
            then 11
            else if x <= 11907
              then 11
              else if x <= 12099
                then 11
                else if x <= 12195
                  then 11
                  else if x <= 12243
                    then 11
                    else if x <= 12267
                      then 11
                      else if x <= 12279
                        then 11
                        else if x <= 12285
                          then 11
                          else if x <= 12288
                            then if x <= 12287
                              then 11
                              else 0
                            else 11
    else 11
discriminatorTrans 1 x =
  if x <= -1
  then -1
  else if x <= 12291
    then 11
    else 11
discriminatorTrans 2 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then 11
                      else if x <= 36
                        then if x <= 30
                          then 11
                          else if x <= 33
                            then 11
                            else if x <= 35
                              then if x <= 34
                                then 1
                                else 11
                              else 11
                        else 11
                    else 11
                  else 11
                else 11
              else 11
            else 11
          else 11
        else 11
      else 11
    else 11
discriminatorTrans 3 x =
  if x <= -1
  then -1
  else if x <= 12291
    then 11
    else 11
discriminatorTrans 4 x =
  if x <= -1
  then -1
  else if x <= 12291
    then 11
    else 11
discriminatorTrans 5 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then 12
                      else if x <= 36
                        then 12
                        else if x <= 42
                          then if x <= 39
                            then 12
                            else if x <= 41
                              then 12
                              else 14
                          else 12
                    else 12
                  else 12
                else 12
              else 12
            else 12
          else 12
        else 12
      else 12
    else 12
discriminatorTrans 6 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then if x <= 12
                        then if x <= 6
                          then 6
                          else if x <= 9
                            then 6
                            else if x <= 11
                              then if x <= 10
                                then 11
                                else 6
                              else 6
                        else 6
                      else 6
                    else 6
                  else 6
                else 6
              else 6
            else 6
          else 6
        else 6
      else 6
    else 6
discriminatorTrans 7 x =
  if x <= -1
  then -1
  else if x <= 12291
    then 11
    else 11
discriminatorTrans 8 x =
  if x <= -1
  then -1
  else if x <= 12291
    then 11
    else 11
discriminatorTrans 9 x =
  if x <= -1
  then -1
  else if x <= 12291
    then 11
    else 11
discriminatorTrans 10 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then if x <= 12
                        then if x <= 6
                          then 11
                          else if x <= 9
                            then if x <= 8
                              then 11
                              else 0
                            else 0
                        else if x <= 18
                          then if x <= 15
                            then if x <= 14
                              then if x <= 13
                                then 0
                                else 11
                              else 11
                            else 11
                          else 11
                      else if x <= 36
                        then if x <= 30
                          then 11
                          else if x <= 33
                            then if x <= 32
                              then if x <= 31
                                then 11
                                else 0
                              else 11
                            else if x <= 35
                              then if x <= 34
                                then 16
                                else 11
                              else 11
                        else if x <= 42
                          then if x <= 39
                            then 11
                            else if x <= 41
                              then if x <= 40
                                then 3
                                else 4
                              else 11
                          else if x <= 45
                            then 11
                            else if x <= 47
                              then if x <= 46
                                then 11
                                else 13
                              else 11
                    else if x <= 72
                      then 11
                      else if x <= 84
                        then if x <= 78
                          then if x <= 75
                            then if x <= 74
                              then if x <= 73
                                then 9
                                else 11
                              else 8
                            else 11
                          else if x <= 81
                            then 11
                            else if x <= 83
                              then if x <= 82
                                then 11
                                else 7
                              else 11
                        else 11
                  else if x <= 144
                    then if x <= 120
                      then if x <= 108
                        then if x <= 102
                          then 11
                          else if x <= 105
                            then if x <= 104
                              then 11
                              else 9
                            else if x <= 107
                              then if x <= 106
                                then 11
                                else 8
                              else 11
                        else if x <= 114
                          then 11
                          else if x <= 117
                            then if x <= 116
                              then if x <= 115
                                then 7
                                else 11
                              else 11
                            else 11
                      else 11
                    else if x <= 168
                      then if x <= 156
                        then 11
                        else if x <= 162
                          then if x <= 159
                            then 11
                            else if x <= 161
                              then if x <= 160
                                then 0
                                else 11
                              else 11
                          else 11
                      else 11
                else 11
              else 11
            else 11
          else 11
        else if x <= 4609
          then 11
          else if x <= 5377
            then 11
            else if x <= 5761
              then if x <= 5569
                then 11
                else if x <= 5665
                  then 11
                  else if x <= 5713
                    then 11
                    else if x <= 5737
                      then 11
                      else if x <= 5749
                        then 11
                        else if x <= 5755
                          then 11
                          else if x <= 5758
                            then 11
                            else if x <= 5760
                              then if x <= 5759
                                then 11
                                else 0
                              else 11
              else 11
      else if x <= 9218
        then if x <= 7682
          then 11
          else if x <= 8450
            then if x <= 8066
              then 11
              else if x <= 8258
                then if x <= 8162
                  then 11
                  else if x <= 8210
                    then if x <= 8186
                      then 11
                      else if x <= 8198
                        then if x <= 8192
                          then if x <= 8189
                            then 11
                            else if x <= 8191
                              then 11
                              else 0
                          else 0
                        else if x <= 8204
                          then if x <= 8201
                            then 0
                            else if x <= 8203
                              then if x <= 8202
                                then 0
                                else 11
                              else 11
                          else 11
                    else if x <= 8234
                      then 11
                      else if x <= 8246
                        then if x <= 8240
                          then if x <= 8237
                            then 11
                            else if x <= 8239
                              then if x <= 8238
                                then 11
                                else 0
                              else 11
                          else 11
                        else 11
                else if x <= 8354
                  then if x <= 8306
                    then if x <= 8282
                      then 11
                      else if x <= 8294
                        then if x <= 8288
                          then if x <= 8285
                            then 11
                            else if x <= 8287
                              then if x <= 8286
                                then 11
                                else 0
                              else 11
                          else 11
                        else 11
                    else 11
                  else 11
            else 11
        else if x <= 10755
          then 11
          else if x <= 11523
            then 11
            else if x <= 11907
              then 11
              else if x <= 12099
                then 11
                else if x <= 12195
                  then 11
                  else if x <= 12243
                    then 11
                    else if x <= 12267
                      then 11
                      else if x <= 12279
                        then 11
                        else if x <= 12285
                          then 11
                          else if x <= 12288
                            then if x <= 12287
                              then 11
                              else 0
                            else 11
    else 11
discriminatorTrans 12 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then 12
                      else if x <= 36
                        then 12
                        else if x <= 42
                          then if x <= 39
                            then 12
                            else if x <= 41
                              then 12
                              else 14
                          else 12
                    else 12
                  else 12
                else 12
              else 12
            else 12
          else 12
        else 12
      else 12
    else 12
discriminatorTrans 13 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then 11
                      else if x <= 36
                        then 11
                        else if x <= 42
                          then if x <= 39
                            then 11
                            else if x <= 41
                              then 11
                              else 12
                          else if x <= 45
                            then 11
                            else if x <= 47
                              then if x <= 46
                                then 11
                                else 6
                              else 11
                    else 11
                  else 11
                else 11
              else 11
            else 11
          else 11
        else 11
      else 11
    else 11
discriminatorTrans 14 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then 12
                      else if x <= 36
                        then 12
                        else if x <= 42
                          then if x <= 39
                            then 12
                            else if x <= 41
                              then 12
                              else 14
                          else if x <= 45
                            then 12
                            else if x <= 47
                              then if x <= 46
                                then 12
                                else 5
                              else 12
                    else 12
                  else 12
                else 12
              else 12
            else 12
          else 12
        else 12
      else 12
    else 12
discriminatorTrans 15 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then 11
                      else if x <= 36
                        then if x <= 30
                          then 11
                          else if x <= 33
                            then 11
                            else if x <= 35
                              then if x <= 34
                                then 16
                                else 11
                              else 11
                        else 11
                    else if x <= 72
                      then 11
                      else if x <= 84
                        then 11
                        else if x <= 90
                          then 11
                          else if x <= 93
                            then if x <= 92
                              then if x <= 91
                                then 11
                                else 16
                              else 11
                            else 11
                  else 11
                else 11
              else 11
            else 11
          else 11
        else 11
      else 11
    else 11
discriminatorTrans 16 x =
  if x <= -1
  then -1
  else if x <= 12291
    then if x <= 6145
      then if x <= 3072
        then if x <= 1536
          then if x <= 768
            then if x <= 384
              then if x <= 192
                then if x <= 96
                  then if x <= 48
                    then if x <= 24
                      then 16
                      else if x <= 36
                        then if x <= 30
                          then 16
                          else if x <= 33
                            then 16
                            else if x <= 35
                              then if x <= 34
                                then 2
                                else 16
                              else 16
                        else 16
                    else if x <= 72
                      then 16
                      else if x <= 84
                        then 16
                        else if x <= 90
                          then 16
                          else if x <= 93
                            then if x <= 92
                              then if x <= 91
                                then 16
                                else 15
                              else 16
                            else 16
                  else 16
                else 16
              else 16
            else 16
          else 16
        else 16
      else 16
    else 16
discriminatorTrans _ _ = -1

-- |Decide sin una categoría léxica debe quedarse o ser ignorada
keepLexCat :: LexCat -> Bool
keepLexCat (White_tk _) = False
keepLexCat (Comnt_tk _) = False
keepLexCat (Mcomt_tk _) = False
keepLexCat _ = True

-- |Un lexer toma una cadena y devuelve una lista de tokens o error
lexer :: String -> Either String [Token]
lexer sIn =
   let
       prefixLookahead :: [String] -> String -> Maybe (String, String)
       prefixLookahead [] _ = Nothing
       prefixLookahead (p:ps) str
           | p `isPrefixOf` str = Just (p, drop (length p) str)
           | otherwise = prefixLookahead ps str
       blength :: String -> Int
       blength = BSTR.length . ENC.encodeUtf8 . TXT.pack
       chbyt :: Char -> Int
       chbyt c = BSTR.length $ ENC.encodeUtf8 $ TXT.singleton c
       advanceMeta [] lin col idx byt = (lin,col,idx,byt)
       advanceMeta s@(x:xs) lin col idx byt
           | Just (newl, rest) <- prefixLookahead ["\r\n", "\r", "\n"] s =
               advanceMeta rest (lin+1) 1 (idx + length newl) (byt + blength newl)
           | otherwise = advanceMeta xs lin (col+1) (idx+1) (byt+chbyt x)
       aux _ _ _ _ [] acc = Right $ reverse acc
       aux lin col idx byt s acc
           | Just (rest, tok) <- discr s =
               let
                   lexeme = lcStr tok
                   (lin',col',idx',byt') = advanceMeta lexeme lin col idx byt
                   meta = MetaToken lin col idx byt
               in aux lin' col' idx' byt' rest $
                      if keepLexCat tok
                      then Token meta tok : acc
                      else acc
           | otherwise =
               let
                   errsymb = takeWhile (not . isSpace) s
                   title = "[ERROR (Lexer)]: "
                   errmsg = "Error léxico en el símbolo `" ++ errsymb ++ "` "
                   linecol = "L:" ++ show lin ++ " C:" ++ show col
                   charbyt = "Char#" ++ show idx ++ " Byte#" ++ show byt
                   infopos = "[en: " ++ linecol ++ ", i.e. " ++ charbyt ++ "]"
               in Left $ title ++ errmsg ++ infopos
   in aux 1 1 0 0 sIn []

-- |Hace lo que el lexer pero lo muestra en formato legible para un humano
lexerShow :: String -> String
lexerShow s =
   let aux (Left errMsg) = "ERROR\n\n" ++ errMsg
       aux (Right ts) = intercalate "\n" $ map show $ ts
   in aux $ lexer s
