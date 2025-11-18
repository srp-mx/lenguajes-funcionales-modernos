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
data LexCat = Def_eq_tk String
            | Semicl_tk String
            | Par_op_tk String
            | Par_cl_tk String
            | Bra_op_tk String
            | Bra_cl_tk String
            | String_tk String
            | White_tk String
            | Comnt_tk String
            | Mcomt_tk String
            | Id_tk String
   deriving (Show,Eq,Ord)

-- |Aplica la reversa de un lexema
revLexCat :: LexCat -> LexCat
revLexCat (Def_eq_tk s) = Def_eq_tk $ reverse s
revLexCat (Semicl_tk s) = Semicl_tk $ reverse s
revLexCat (Par_op_tk s) = Par_op_tk $ reverse s
revLexCat (Par_cl_tk s) = Par_cl_tk $ reverse s
revLexCat (Bra_op_tk s) = Bra_op_tk $ reverse s
revLexCat (Bra_cl_tk s) = Bra_cl_tk $ reverse s
revLexCat (String_tk s) = String_tk $ reverse s
revLexCat (White_tk s) = White_tk $ reverse s
revLexCat (Comnt_tk s) = Comnt_tk $ reverse s
revLexCat (Mcomt_tk s) = Mcomt_tk $ reverse s
revLexCat (Id_tk s) = Id_tk $ reverse s

-- |Extrae la cadena del lexema
lcStr :: LexCat -> String
lcStr (Def_eq_tk s) = s
lcStr (Semicl_tk s) = s
lcStr (Par_op_tk s) = s
lcStr (Par_cl_tk s) = s
lcStr (Bra_op_tk s) = s
lcStr (Bra_cl_tk s) = s
lcStr (String_tk s) = s
lcStr (White_tk s) = s
lcStr (Comnt_tk s) = s
lcStr (Mcomt_tk s) = s
lcStr (Id_tk s) = s

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
discr s = discriminator s 12 [] Nothing

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
    Just ([], Semicl_tk $ reverse accS)
discriminator (x:xs) 3 accS _ =
    discriminator xs (discTrans 3 x) (x:accS) $
        Just (x:xs, Semicl_tk $ accS)
discriminator [] 4 accS _ =
    Just ([], Par_op_tk $ reverse accS)
discriminator (x:xs) 4 accS _ =
    discriminator xs (discTrans 4 x) (x:accS) $
        Just (x:xs, Par_op_tk $ accS)
discriminator [] 5 accS _ =
    Just ([], Par_cl_tk $ reverse accS)
discriminator (x:xs) 5 accS _ =
    discriminator xs (discTrans 5 x) (x:accS) $
        Just (x:xs, Par_cl_tk $ accS)
discriminator [] 6 accS _ =
    Just ([], Mcomt_tk $ reverse accS)
discriminator (x:xs) 6 accS _ =
    discriminator xs (discTrans 6 x) (x:accS) $
        Just (x:xs, Mcomt_tk $ accS)
discriminator [] 7 accS _ =
    Just ([], Id_tk $ reverse accS)
discriminator (x:xs) 7 accS _ =
    discriminator xs (discTrans 7 x) (x:accS) $
        Just (x:xs, Id_tk $ accS)
discriminator [] 8 accS _ =
    Just ([], Def_eq_tk $ reverse accS)
discriminator (x:xs) 8 accS _ =
    discriminator xs (discTrans 8 x) (x:accS) $
        Just (x:xs, Def_eq_tk $ accS)
discriminator [] 9 accS _ =
    Just ([], Comnt_tk $ reverse accS)
discriminator (x:xs) 9 accS _ =
    discriminator xs (discTrans 9 x) (x:accS) $
        Just (x:xs, Comnt_tk $ accS)
discriminator [] 10 accS _ =
    Just ([], Bra_op_tk $ reverse accS)
discriminator (x:xs) 10 accS _ =
    discriminator xs (discTrans 10 x) (x:accS) $
        Just (x:xs, Bra_op_tk $ accS)
discriminator [] 11 accS _ =
    Just ([], Bra_cl_tk $ reverse accS)
discriminator (x:xs) 11 accS _ =
    discriminator xs (discTrans 11 x) (x:accS) $
        Just (x:xs, Bra_cl_tk $ accS)
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
discriminator (x:xs) 18 accS accC =
    discriminator xs (discTrans 18 x) (x:accS) accC
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
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then if x <= 15
                              then if x <= 7
                                then 17
                                else if x <= 11
                                  then if x <= 9
                                    then if x <= 8
                                      then 17
                                      else 0
                                    else 0
                                  else if x <= 13
                                    then 0
                                    else 17
                              else 17
                            else if x <= 46
                              then if x <= 38
                                then if x <= 34
                                  then if x <= 32
                                    then if x <= 31
                                      then 17
                                      else 0
                                    else 17
                                  else 17
                                else 17
                              else 17
                          else 17
                        else if x <= 183
                          then if x <= 153
                            then 17
                            else if x <= 168
                              then if x <= 161
                                then if x <= 157
                                  then 17
                                  else if x <= 159
                                    then 17
                                    else if x <= 160
                                      then 0
                                      else 17
                                else 17
                              else 17
                          else 17
                      else 17
                    else 17
                  else 17
                else 17
              else if x <= 5871
                then if x <= 4893
                  then 17
                  else if x <= 5382
                    then 17
                    else if x <= 5627
                      then 17
                      else if x <= 5749
                        then 17
                        else if x <= 5810
                          then if x <= 5780
                            then if x <= 5765
                              then if x <= 5757
                                then 17
                                else if x <= 5761
                                  then if x <= 5759
                                    then 17
                                    else if x <= 5760
                                      then 0
                                      else 17
                                  else 17
                              else 17
                            else 17
                          else 17
                else 17
            else if x <= 11742
              then if x <= 9785
                then if x <= 8807
                  then if x <= 8318
                    then if x <= 8073
                      then 17
                      else if x <= 8196
                        then if x <= 8135
                          then 17
                          else if x <= 8166
                            then 17
                            else if x <= 8181
                              then 17
                              else if x <= 8189
                                then 17
                                else if x <= 8193
                                  then if x <= 8191
                                    then 17
                                    else 0
                                  else 0
                        else if x <= 8257
                          then if x <= 8227
                            then if x <= 8212
                              then if x <= 8204
                                then if x <= 8200
                                  then 0
                                  else if x <= 8202
                                    then 0
                                    else 17
                                else 17
                              else 17
                            else if x <= 8242
                              then if x <= 8235
                                then 17
                                else if x <= 8239
                                  then if x <= 8237
                                    then 17
                                    else if x <= 8238
                                      then 17
                                      else 0
                                  else 17
                              else 17
                          else if x <= 8288
                            then if x <= 8273
                              then 17
                              else if x <= 8281
                                then 17
                                else if x <= 8285
                                  then 17
                                  else if x <= 8287
                                    then if x <= 8286
                                      then 17
                                      else 0
                                    else 17
                            else 17
                    else 17
                  else 17
                else 17
              else if x <= 13699
                then if x <= 12721
                  then if x <= 12232
                    then 17
                    else if x <= 12477
                      then if x <= 12355
                        then if x <= 12294
                          then if x <= 12263
                            then 17
                            else if x <= 12279
                              then 17
                              else if x <= 12287
                                then 17
                                else if x <= 12291
                                  then if x <= 12289
                                    then if x <= 12288
                                      then 0
                                      else 17
                                    else 17
                                  else 17
                          else 17
                        else 17
                      else 17
                  else 17
                else 17
          else 17
        else 17
      else 17
    else 17
discriminatorTrans 1 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then 17
                            else if x <= 46
                              then if x <= 38
                                then if x <= 34
                                  then if x <= 32
                                    then 17
                                    else if x <= 33
                                      then 17
                                      else 2
                                  else 17
                                else 17
                              else 17
                          else 17
                        else 17
                      else 17
                    else 17
                  else 17
                else 17
              else 17
            else 17
          else 17
        else 17
      else 17
    else 17
discriminatorTrans 2 x =
  if x <= -1
  then -1
  else if x <= 125254
    then 17
    else 17
discriminatorTrans 3 x =
  if x <= -1
  then -1
  else if x <= 125254
    then 17
    else 17
discriminatorTrans 4 x =
  if x <= -1
  then -1
  else if x <= 125254
    then 17
    else 17
discriminatorTrans 5 x =
  if x <= -1
  then -1
  else if x <= 125254
    then 17
    else 17
discriminatorTrans 6 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then 18
                            else if x <= 46
                              then if x <= 38
                                then 18
                                else if x <= 42
                                  then if x <= 40
                                    then 18
                                    else if x <= 41
                                      then 18
                                      else 16
                                  else 18
                              else 18
                          else 18
                        else 18
                      else 18
                    else 18
                  else 18
                else 18
              else 18
            else 18
          else 18
        else 18
      else 18
    else 18
discriminatorTrans 7 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then 17
                            else if x <= 46
                              then if x <= 38
                                then 17
                                else if x <= 42
                                  then if x <= 40
                                    then if x <= 39
                                      then 7
                                      else 17
                                    else 17
                                  else if x <= 44
                                    then 17
                                    else 7
                              else if x <= 54
                                then if x <= 50
                                  then if x <= 48
                                    then if x <= 47
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else if x <= 58
                                  then if x <= 56
                                    then 7
                                    else if x <= 57
                                      then 7
                                      else 17
                                  else 17
                          else if x <= 92
                            then if x <= 77
                              then if x <= 69
                                then if x <= 65
                                  then if x <= 63
                                    then if x <= 62
                                      then 17
                                      else 7
                                    else if x <= 64
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else if x <= 85
                                then 7
                                else if x <= 89
                                  then 7
                                  else if x <= 91
                                    then if x <= 90
                                      then 7
                                      else 17
                                    else 17
                            else if x <= 107
                              then if x <= 100
                                then if x <= 96
                                  then if x <= 94
                                    then 17
                                    else if x <= 95
                                      then 7
                                      else 17
                                  else 7
                                else 7
                              else 7
                        else if x <= 183
                          then if x <= 153
                            then 17
                            else if x <= 168
                              then 17
                              else if x <= 176
                                then 17
                                else if x <= 180
                                  then 17
                                  else if x <= 182
                                    then if x <= 181
                                      then 7
                                      else 17
                                    else 17
                          else if x <= 214
                            then if x <= 199
                              then if x <= 191
                                then 17
                                else 7
                              else 7
                            else if x <= 229
                              then if x <= 222
                                then if x <= 218
                                  then if x <= 216
                                    then if x <= 215
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else 7
                              else 7
                      else if x <= 367
                        then if x <= 306
                          then if x <= 275
                            then if x <= 260
                              then if x <= 252
                                then if x <= 248
                                  then if x <= 246
                                    then 7
                                    else if x <= 247
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else 7
                            else 7
                          else 7
                        else if x <= 428
                          then 7
                          else if x <= 459
                            then if x <= 444
                              then if x <= 436
                                then 7
                                else if x <= 440
                                  then 7
                                  else if x <= 442
                                    then 7
                                    else if x <= 443
                                      then 17
                                      else 7
                              else if x <= 452
                                then if x <= 448
                                  then if x <= 446
                                    then 7
                                    else if x <= 447
                                      then 7
                                      else 17
                                  else if x <= 450
                                    then 17
                                    else if x <= 451
                                      then 17
                                      else 7
                                else if x <= 456
                                  then if x <= 454
                                    then if x <= 453
                                      then 17
                                      else 7
                                    else if x <= 455
                                      then 7
                                      else 17
                                  else if x <= 458
                                    then 7
                                    else 17
                            else 7
                    else if x <= 734
                      then if x <= 612
                        then if x <= 551
                          then if x <= 520
                            then if x <= 505
                              then if x <= 497
                                then 7
                                else if x <= 501
                                  then if x <= 499
                                    then if x <= 498
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                              else 7
                            else 7
                          else 7
                        else if x <= 673
                          then if x <= 643
                            then 7
                            else if x <= 658
                              then 7
                              else if x <= 666
                                then if x <= 662
                                  then if x <= 660
                                    then if x <= 659
                                      then 7
                                      else 17
                                    else 7
                                  else 7
                                else 7
                          else if x <= 704
                            then if x <= 689
                              then if x <= 681
                                then 7
                                else if x <= 685
                                  then 7
                                  else if x <= 687
                                    then 7
                                    else 17
                              else 17
                            else 17
                      else if x <= 856
                        then 17
                        else if x <= 917
                          then if x <= 887
                            then if x <= 872
                              then 17
                              else if x <= 880
                                then if x <= 876
                                  then 17
                                  else if x <= 878
                                    then 17
                                    else if x <= 879
                                      then 17
                                      else 7
                                else if x <= 884
                                  then if x <= 882
                                    then 7
                                    else if x <= 883
                                      then 7
                                      else 17
                                  else if x <= 886
                                    then if x <= 885
                                      then 17
                                      else 7
                                    else 7
                            else if x <= 902
                              then if x <= 895
                                then if x <= 891
                                  then if x <= 889
                                    then 17
                                    else if x <= 890
                                      then 17
                                      else 7
                                  else if x <= 893
                                    then 7
                                    else if x <= 894
                                      then 17
                                      else 7
                                else if x <= 899
                                  then 17
                                  else if x <= 901
                                    then 17
                                    else 7
                              else if x <= 910
                                then if x <= 906
                                  then if x <= 904
                                    then if x <= 903
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 908
                                    then if x <= 907
                                      then 17
                                      else 7
                                    else if x <= 909
                                      then 17
                                      else 7
                                else 7
                          else if x <= 948
                            then if x <= 933
                              then if x <= 925
                                then 7
                                else if x <= 929
                                  then 7
                                  else if x <= 931
                                    then if x <= 930
                                      then 17
                                      else 7
                                    else 7
                              else 7
                            else 7
                  else if x <= 1468
                    then if x <= 1223
                      then if x <= 1101
                        then if x <= 1040
                          then if x <= 1009
                            then 7
                            else if x <= 1025
                              then if x <= 1017
                                then if x <= 1013
                                  then 7
                                  else if x <= 1015
                                    then if x <= 1014
                                      then 17
                                      else 7
                                    else 7
                                else 7
                              else 7
                          else 7
                        else if x <= 1162
                          then if x <= 1132
                            then 7
                            else if x <= 1147
                              then 7
                              else if x <= 1155
                                then if x <= 1151
                                  then 7
                                  else if x <= 1153
                                    then 7
                                    else 17
                                else if x <= 1159
                                  then 17
                                  else if x <= 1161
                                    then 17
                                    else 7
                          else 7
                      else if x <= 1346
                        then if x <= 1285
                          then 7
                          else if x <= 1316
                            then 7
                            else if x <= 1331
                              then if x <= 1324
                                then 7
                                else if x <= 1328
                                  then if x <= 1326
                                    then 7
                                    else if x <= 1327
                                      then 7
                                      else 17
                                  else 7
                              else 7
                        else if x <= 1407
                          then if x <= 1377
                            then if x <= 1362
                              then 7
                              else if x <= 1370
                                then if x <= 1366
                                  then 7
                                  else 17
                                else if x <= 1374
                                  then 17
                                  else if x <= 1376
                                    then if x <= 1375
                                      then 17
                                      else 7
                                    else 7
                            else 7
                          else if x <= 1438
                            then if x <= 1423
                              then if x <= 1415
                                then 7
                                else if x <= 1419
                                  then if x <= 1417
                                    then if x <= 1416
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                              else 17
                            else 17
                    else 17
                else 17
              else if x <= 5871
                then if x <= 4893
                  then if x <= 4404
                    then if x <= 4159
                      then 17
                      else if x <= 4282
                        then if x <= 4221
                          then 17
                          else if x <= 4252
                            then 17
                            else if x <= 4267
                              then if x <= 4260
                                then if x <= 4256
                                  then if x <= 4254
                                    then 17
                                    else if x <= 4255
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else 7
                        else if x <= 4343
                          then if x <= 4313
                            then if x <= 4298
                              then if x <= 4290
                                then 7
                                else if x <= 4294
                                  then if x <= 4292
                                    then 7
                                    else if x <= 4293
                                      then 7
                                      else 17
                                  else if x <= 4296
                                    then if x <= 4295
                                      then 7
                                      else 17
                                    else 17
                              else if x <= 4306
                                then if x <= 4302
                                  then if x <= 4300
                                    then 17
                                    else if x <= 4301
                                      then 7
                                      else 17
                                  else if x <= 4304
                                    then if x <= 4303
                                      then 17
                                      else 7
                                    else 7
                                else 7
                            else 7
                          else if x <= 4374
                            then if x <= 4359
                              then if x <= 4351
                                then if x <= 4347
                                  then if x <= 4345
                                    then 7
                                    else if x <= 4346
                                      then 7
                                      else 17
                                  else if x <= 4349
                                    then if x <= 4348
                                      then 17
                                      else 7
                                    else 7
                                else 17
                              else 17
                            else 17
                    else 17
                  else if x <= 5382
                    then if x <= 5138
                      then if x <= 5016
                        then 17
                        else if x <= 5077
                          then if x <= 5047
                            then if x <= 5032
                              then if x <= 5024
                                then if x <= 5020
                                  then 17
                                  else if x <= 5022
                                    then 17
                                    else if x <= 5023
                                      then 17
                                      else 7
                                else 7
                              else 7
                            else 7
                          else if x <= 5108
                            then 7
                            else if x <= 5123
                              then if x <= 5116
                                then if x <= 5112
                                  then if x <= 5110
                                    then if x <= 5109
                                      then 7
                                      else 17
                                    else if x <= 5111
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 5120
                                  then if x <= 5118
                                    then if x <= 5117
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                              else 17
                      else 17
                    else 17
                else if x <= 6850
                  then 17
                  else if x <= 7339
                    then if x <= 7095
                      then 17
                      else if x <= 7217
                        then 17
                        else if x <= 7278
                          then 17
                          else if x <= 7309
                            then if x <= 7294
                              then 17
                              else if x <= 7302
                                then if x <= 7298
                                  then if x <= 7296
                                    then if x <= 7295
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else if x <= 7306
                                  then if x <= 7304
                                    then 7
                                    else 17
                                  else 17
                            else if x <= 7324
                              then if x <= 7317
                                then if x <= 7313
                                  then if x <= 7311
                                    then 17
                                    else 7
                                  else 7
                                else 7
                              else 7
                    else if x <= 7584
                      then if x <= 7462
                        then if x <= 7401
                          then if x <= 7370
                            then if x <= 7355
                              then if x <= 7347
                                then 7
                                else if x <= 7351
                                  then 7
                                  else if x <= 7353
                                    then 7
                                    else if x <= 7354
                                      then 7
                                      else 17
                              else if x <= 7363
                                then if x <= 7359
                                  then if x <= 7357
                                    then if x <= 7356
                                      then 17
                                      else 7
                                    else 7
                                  else 17
                                else 17
                            else 17
                          else if x <= 7432
                            then if x <= 7417
                              then 17
                              else if x <= 7425
                                then if x <= 7421
                                  then 17
                                  else if x <= 7423
                                    then 17
                                    else 7
                                else 7
                            else 7
                        else if x <= 7523
                          then if x <= 7493
                            then if x <= 7478
                              then if x <= 7470
                                then if x <= 7466
                                  then 7
                                  else if x <= 7468
                                    then if x <= 7467
                                      then 7
                                      else 17
                                    else 17
                                else 17
                              else 17
                            else 17
                          else if x <= 7554
                            then if x <= 7539
                              then if x <= 7531
                                then if x <= 7527
                                  then 17
                                  else if x <= 7529
                                    then 17
                                    else if x <= 7530
                                      then 17
                                      else 7
                                else 7
                              else if x <= 7547
                                then if x <= 7543
                                  then 7
                                  else if x <= 7545
                                    then if x <= 7544
                                      then 17
                                      else 7
                                    else 7
                                else 7
                            else if x <= 7569
                              then 7
                              else if x <= 7577
                                then 7
                                else if x <= 7581
                                  then if x <= 7579
                                    then if x <= 7578
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                      else if x <= 7706
                        then if x <= 7645
                          then 17
                          else if x <= 7676
                            then 17
                            else if x <= 7691
                              then if x <= 7684
                                then if x <= 7680
                                  then if x <= 7678
                                    then 17
                                    else if x <= 7679
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else 7
                        else 7
            else if x <= 11742
              then if x <= 9785
                then if x <= 8807
                  then if x <= 8318
                    then if x <= 8073
                      then if x <= 7951
                        then 7
                        else if x <= 8012
                          then if x <= 7982
                            then if x <= 7967
                              then if x <= 7959
                                then if x <= 7955
                                  then 7
                                  else if x <= 7957
                                    then 7
                                    else 17
                                else if x <= 7963
                                  then 7
                                  else if x <= 7965
                                    then 7
                                    else 17
                              else 7
                            else if x <= 7997
                              then 7
                              else if x <= 8005
                                then 7
                                else if x <= 8009
                                  then if x <= 8007
                                    then 17
                                    else 7
                                  else 7
                          else if x <= 8043
                            then if x <= 8028
                              then if x <= 8020
                                then if x <= 8016
                                  then if x <= 8014
                                    then if x <= 8013
                                      then 7
                                      else 17
                                    else if x <= 8015
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 8024
                                  then if x <= 8022
                                    then 7
                                    else if x <= 8023
                                      then 7
                                      else 17
                                  else if x <= 8026
                                    then if x <= 8025
                                      then 7
                                      else 17
                                    else if x <= 8027
                                      then 7
                                      else 17
                              else if x <= 8036
                                then if x <= 8032
                                  then if x <= 8030
                                    then if x <= 8029
                                      then 7
                                      else 17
                                    else 7
                                  else 7
                                else 7
                            else if x <= 8058
                              then 7
                              else if x <= 8066
                                then if x <= 8062
                                  then if x <= 8060
                                    then 7
                                    else if x <= 8061
                                      then 7
                                      else 17
                                  else if x <= 8064
                                    then if x <= 8063
                                      then 17
                                      else 7
                                    else 7
                                else if x <= 8070
                                  then 7
                                  else if x <= 8072
                                    then if x <= 8071
                                      then 7
                                      else 17
                                    else 17
                      else if x <= 8196
                        then if x <= 8135
                          then if x <= 8104
                            then if x <= 8089
                              then if x <= 8081
                                then if x <= 8077
                                  then 17
                                  else if x <= 8079
                                    then 17
                                    else 7
                                else if x <= 8085
                                  then 7
                                  else if x <= 8087
                                    then 7
                                    else 17
                              else if x <= 8097
                                then if x <= 8093
                                  then 17
                                  else if x <= 8095
                                    then 17
                                    else 7
                                else if x <= 8101
                                  then 7
                                  else if x <= 8103
                                    then 7
                                    else 17
                            else if x <= 8120
                              then if x <= 8112
                                then if x <= 8108
                                  then 17
                                  else if x <= 8110
                                    then 17
                                    else if x <= 8111
                                      then 17
                                      else 7
                                else if x <= 8116
                                  then 7
                                  else if x <= 8118
                                    then if x <= 8117
                                      then 17
                                      else 7
                                    else 7
                              else if x <= 8128
                                then if x <= 8124
                                  then if x <= 8122
                                    then 7
                                    else if x <= 8123
                                      then 7
                                      else 17
                                  else if x <= 8126
                                    then if x <= 8125
                                      then 17
                                      else 7
                                    else 17
                                else if x <= 8132
                                  then if x <= 8130
                                    then if x <= 8129
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 8134
                                    then if x <= 8133
                                      then 17
                                      else 7
                                    else 7
                          else if x <= 8166
                            then if x <= 8151
                              then if x <= 8143
                                then if x <= 8139
                                  then 7
                                  else 17
                                else if x <= 8147
                                  then 7
                                  else if x <= 8149
                                    then 17
                                    else 7
                              else if x <= 8159
                                then if x <= 8155
                                  then 7
                                  else 17
                                else 7
                            else if x <= 8181
                              then if x <= 8174
                                then if x <= 8170
                                  then 7
                                  else if x <= 8172
                                    then 7
                                    else 17
                                else if x <= 8178
                                  then if x <= 8176
                                    then 17
                                    else if x <= 8177
                                      then 17
                                      else 7
                                  else if x <= 8180
                                    then 7
                                    else 17
                              else if x <= 8189
                                then if x <= 8185
                                  then 7
                                  else if x <= 8187
                                    then 7
                                    else 17
                                else 17
                        else 17
                    else if x <= 8563
                      then if x <= 8441
                        then 17
                        else if x <= 8502
                          then if x <= 8472
                            then if x <= 8457
                              then if x <= 8449
                                then 17
                                else if x <= 8453
                                  then if x <= 8451
                                    then if x <= 8450
                                      then 7
                                      else 17
                                    else 17
                                  else if x <= 8455
                                    then if x <= 8454
                                      then 17
                                      else 7
                                    else 17
                              else if x <= 8465
                                then 7
                                else if x <= 8469
                                  then if x <= 8467
                                    then 7
                                    else if x <= 8468
                                      then 17
                                      else 7
                                  else 17
                            else if x <= 8487
                              then if x <= 8480
                                then if x <= 8476
                                  then 7
                                  else if x <= 8478
                                    then if x <= 8477
                                      then 7
                                      else 17
                                    else 17
                                else if x <= 8484
                                  then if x <= 8482
                                    then 17
                                    else if x <= 8483
                                      then 17
                                      else 7
                                  else if x <= 8486
                                    then if x <= 8485
                                      then 17
                                      else 7
                                    else 17
                              else if x <= 8495
                                then if x <= 8491
                                  then if x <= 8489
                                    then if x <= 8488
                                      then 7
                                      else 17
                                    else 7
                                  else if x <= 8493
                                    then 7
                                    else if x <= 8494
                                      then 17
                                      else 7
                                else if x <= 8499
                                  then 7
                                  else if x <= 8501
                                    then if x <= 8500
                                      then 7
                                      else 17
                                    else 17
                          else if x <= 8533
                            then if x <= 8518
                              then if x <= 8510
                                then if x <= 8506
                                  then if x <= 8504
                                    then 17
                                    else if x <= 8505
                                      then 7
                                      else 17
                                  else if x <= 8508
                                    then if x <= 8507
                                      then 17
                                      else 7
                                    else 7
                                else if x <= 8514
                                  then if x <= 8512
                                    then if x <= 8511
                                      then 7
                                      else 17
                                    else 17
                                  else if x <= 8516
                                    then 17
                                    else 7
                              else if x <= 8526
                                then if x <= 8522
                                  then if x <= 8520
                                    then 7
                                    else if x <= 8521
                                      then 7
                                      else 17
                                  else if x <= 8524
                                    then 17
                                    else if x <= 8525
                                      then 17
                                      else 7
                                else 17
                            else 17
                      else if x <= 8685
                        then if x <= 8624
                          then if x <= 8594
                            then if x <= 8579
                              then if x <= 8571
                                then 17
                                else if x <= 8575
                                  then 17
                                  else if x <= 8577
                                    then 17
                                    else if x <= 8578
                                      then 17
                                      else 7
                              else if x <= 8587
                                then if x <= 8583
                                  then if x <= 8581
                                    then if x <= 8580
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                                else 17
                            else 17
                          else 17
                        else 17
                  else 17
                else if x <= 10764
                  then 17
                  else if x <= 11253
                    then 17
                    else if x <= 11498
                      then if x <= 11376
                        then if x <= 11315
                          then if x <= 11284
                            then if x <= 11269
                              then if x <= 11261
                                then 17
                                else if x <= 11265
                                  then if x <= 11263
                                    then 17
                                    else 7
                                  else 7
                              else 7
                            else 7
                          else 7
                        else if x <= 11437
                          then if x <= 11407
                            then if x <= 11392
                              then if x <= 11384
                                then 7
                                else if x <= 11388
                                  then if x <= 11386
                                    then 7
                                    else if x <= 11387
                                      then 7
                                      else 17
                                  else if x <= 11390
                                    then if x <= 11389
                                      then 17
                                      else 7
                                    else 7
                              else 7
                            else 7
                          else if x <= 11468
                            then 7
                            else if x <= 11483
                              then 7
                              else if x <= 11491
                                then 7
                                else if x <= 11495
                                  then if x <= 11493
                                    then if x <= 11492
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                      else if x <= 11620
                        then if x <= 11559
                          then if x <= 11529
                            then if x <= 11514
                              then if x <= 11506
                                then if x <= 11502
                                  then 7
                                  else if x <= 11504
                                    then 17
                                    else if x <= 11505
                                      then 17
                                      else 7
                                else if x <= 11510
                                  then if x <= 11508
                                    then if x <= 11507
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                              else if x <= 11522
                                then if x <= 11518
                                  then 17
                                  else if x <= 11520
                                    then if x <= 11519
                                      then 17
                                      else 7
                                    else 7
                                else 7
                            else if x <= 11544
                              then 7
                              else if x <= 11552
                                then 7
                                else if x <= 11556
                                  then 7
                                  else if x <= 11558
                                    then if x <= 11557
                                      then 7
                                      else 17
                                    else 7
                          else if x <= 11590
                            then if x <= 11575
                              then if x <= 11567
                                then if x <= 11563
                                  then 17
                                  else if x <= 11565
                                    then if x <= 11564
                                      then 17
                                      else 7
                                    else 17
                                else 17
                              else 17
                            else 17
                        else 17
              else 17
          else 17
        else if x <= 46970
          then if x <= 39142
            then 17
            else if x <= 43056
              then if x <= 41099
                then 17
                else if x <= 42078
                  then 17
                  else if x <= 42567
                    then if x <= 42323
                      then 17
                      else if x <= 42445
                        then 17
                        else if x <= 42506
                          then 17
                          else if x <= 42537
                            then 17
                            else if x <= 42552
                              then 17
                              else if x <= 42560
                                then if x <= 42556
                                  then 17
                                  else if x <= 42558
                                    then 17
                                    else if x <= 42559
                                      then 17
                                      else 7
                                else 7
                    else if x <= 42812
                      then if x <= 42690
                        then if x <= 42629
                          then if x <= 42598
                            then 7
                            else if x <= 42614
                              then if x <= 42606
                                then if x <= 42602
                                  then 7
                                  else if x <= 42604
                                    then 7
                                    else if x <= 42605
                                      then 7
                                      else 17
                                else 17
                              else if x <= 42622
                                then 17
                                else if x <= 42626
                                  then if x <= 42624
                                    then if x <= 42623
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                          else if x <= 42660
                            then if x <= 42645
                              then 7
                              else if x <= 42653
                                then if x <= 42649
                                  then 7
                                  else if x <= 42651
                                    then 7
                                    else 17
                                else 17
                            else 17
                        else if x <= 42751
                          then 17
                          else if x <= 42782
                            then 17
                            else if x <= 42797
                              then if x <= 42790
                                then if x <= 42786
                                  then if x <= 42784
                                    then 17
                                    else if x <= 42785
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else 7
                      else if x <= 42934
                        then if x <= 42873
                          then if x <= 42843
                            then 7
                            else if x <= 42858
                              then 7
                              else if x <= 42866
                                then if x <= 42862
                                  then 7
                                  else if x <= 42864
                                    then if x <= 42863
                                      then 7
                                      else 17
                                    else 7
                                else 7
                          else if x <= 42904
                            then if x <= 42889
                              then if x <= 42881
                                then 7
                                else if x <= 42885
                                  then 7
                                  else if x <= 42887
                                    then 7
                                    else 17
                              else if x <= 42897
                                then if x <= 42893
                                  then if x <= 42891
                                    then if x <= 42890
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 42895
                                    then if x <= 42894
                                      then 7
                                      else 17
                                    else 7
                                else 7
                            else 7
                        else if x <= 42995
                          then if x <= 42965
                            then if x <= 42950
                              then 7
                              else if x <= 42958
                                then if x <= 42954
                                  then 7
                                  else 17
                                else if x <= 42962
                                  then if x <= 42960
                                    then if x <= 42959
                                      then 17
                                      else 7
                                    else if x <= 42961
                                      then 7
                                      else 17
                                  else if x <= 42964
                                    then if x <= 42963
                                      then 7
                                      else 17
                                    else 7
                            else if x <= 42980
                              then if x <= 42973
                                then if x <= 42969
                                  then 7
                                  else 17
                                else 17
                              else 17
                          else if x <= 43026
                            then if x <= 43011
                              then if x <= 43003
                                then if x <= 42999
                                  then if x <= 42997
                                    then if x <= 42996
                                      then 17
                                      else 7
                                    else if x <= 42998
                                      then 7
                                      else 17
                                  else if x <= 43001
                                    then 17
                                    else if x <= 43002
                                      then 7
                                      else 17
                                else 17
                              else 17
                            else 17
              else if x <= 45013
                then if x <= 44035
                  then if x <= 43546
                    then 17
                    else if x <= 43791
                      then 17
                      else if x <= 43913
                        then if x <= 43852
                          then if x <= 43822
                            then 17
                            else if x <= 43837
                              then if x <= 43830
                                then if x <= 43826
                                  then if x <= 43824
                                    then if x <= 43823
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else 7
                              else 7
                          else if x <= 43883
                            then if x <= 43868
                              then if x <= 43860
                                then 7
                                else if x <= 43864
                                  then 7
                                  else if x <= 43866
                                    then 7
                                    else 17
                              else if x <= 43876
                                then if x <= 43872
                                  then if x <= 43870
                                    then 17
                                    else if x <= 43871
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 43880
                                  then 7
                                  else 17
                            else if x <= 43898
                              then if x <= 43891
                                then if x <= 43887
                                  then 17
                                  else 7
                                else 7
                              else 7
                        else if x <= 43974
                          then if x <= 43944
                            then 7
                            else if x <= 43959
                              then 7
                              else if x <= 43967
                                then 7
                                else 17
                          else 17
                  else 17
                else 17
          else 17
      else if x <= 93941
        then if x <= 78284
          then if x <= 70456
            then if x <= 66542
              then if x <= 64585
                then if x <= 63606
                  then 17
                  else if x <= 64096
                    then 17
                    else if x <= 64341
                      then if x <= 64219
                        then 17
                        else if x <= 64280
                          then if x <= 64250
                            then 17
                            else if x <= 64265
                              then if x <= 64258
                                then if x <= 64254
                                  then 17
                                  else if x <= 64256
                                    then if x <= 64255
                                      then 17
                                      else 7
                                    else 7
                                else if x <= 64262
                                  then 7
                                  else 17
                              else if x <= 64273
                                then 17
                                else if x <= 64277
                                  then if x <= 64275
                                    then if x <= 64274
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 64279
                                    then 7
                                    else 17
                          else 17
                      else 17
                else if x <= 65564
                  then if x <= 65075
                    then 17
                    else if x <= 65320
                      then if x <= 65198
                        then 17
                        else if x <= 65259
                          then 17
                          else if x <= 65290
                            then 17
                            else if x <= 65305
                              then 17
                              else if x <= 65313
                                then if x <= 65309
                                  then 17
                                  else if x <= 65311
                                    then 17
                                    else if x <= 65312
                                      then 17
                                      else 7
                                else 7
                      else if x <= 65442
                        then if x <= 65381
                          then if x <= 65351
                            then if x <= 65336
                              then 7
                              else if x <= 65344
                                then if x <= 65340
                                  then if x <= 65338
                                    then 7
                                    else 17
                                  else 17
                                else 7
                            else if x <= 65366
                              then 7
                              else if x <= 65374
                                then if x <= 65370
                                  then 7
                                  else 17
                                else 17
                          else 17
                        else 17
                  else 17
              else if x <= 68499
                then if x <= 67521
                  then if x <= 67032
                    then if x <= 66787
                      then if x <= 66665
                        then if x <= 66604
                          then if x <= 66573
                            then if x <= 66558
                              then 17
                              else if x <= 66566
                                then if x <= 66562
                                  then if x <= 66560
                                    then if x <= 66559
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else 7
                            else 7
                          else if x <= 66635
                            then 7
                            else if x <= 66650
                              then if x <= 66643
                                then if x <= 66639
                                  then 7
                                  else 17
                                else 17
                              else 17
                        else if x <= 66726
                          then 17
                          else if x <= 66757
                            then if x <= 66742
                              then if x <= 66734
                                then 17
                                else if x <= 66738
                                  then if x <= 66736
                                    then if x <= 66735
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                              else 7
                            else if x <= 66772
                              then if x <= 66765
                                then 7
                                else if x <= 66769
                                  then 7
                                  else if x <= 66771
                                    then 7
                                    else 17
                              else if x <= 66780
                                then if x <= 66776
                                  then if x <= 66774
                                    then 17
                                    else if x <= 66775
                                      then 17
                                      else 7
                                  else 7
                                else 7
                      else if x <= 66910
                        then if x <= 66849
                          then if x <= 66818
                            then if x <= 66803
                              then 7
                              else if x <= 66811
                                then 7
                                else 17
                            else 17
                          else 17
                        else if x <= 66971
                          then if x <= 66941
                            then if x <= 66926
                              then 17
                              else if x <= 66934
                                then if x <= 66930
                                  then if x <= 66928
                                    then if x <= 66927
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else if x <= 66938
                                  then 7
                                  else if x <= 66940
                                    then if x <= 66939
                                      then 17
                                      else 7
                                    else 7
                            else if x <= 66956
                              then if x <= 66949
                                then 7
                                else if x <= 66953
                                  then 7
                                  else if x <= 66955
                                    then if x <= 66954
                                      then 7
                                      else 17
                                    else 7
                              else if x <= 66964
                                then if x <= 66960
                                  then 7
                                  else if x <= 66962
                                    then 7
                                    else if x <= 66963
                                      then 17
                                      else 7
                                else if x <= 66968
                                  then if x <= 66966
                                    then if x <= 66965
                                      then 7
                                      else 17
                                    else 7
                                  else 7
                          else if x <= 67002
                            then if x <= 66987
                              then if x <= 66979
                                then if x <= 66975
                                  then 7
                                  else if x <= 66977
                                    then 7
                                    else if x <= 66978
                                      then 17
                                      else 7
                                else 7
                              else if x <= 66995
                                then if x <= 66991
                                  then 7
                                  else if x <= 66993
                                    then 7
                                    else if x <= 66994
                                      then 17
                                      else 7
                                else if x <= 66999
                                  then 7
                                  else if x <= 67001
                                    then 7
                                    else 17
                            else if x <= 67017
                              then if x <= 67010
                                then if x <= 67006
                                  then if x <= 67004
                                    then 7
                                    else 17
                                  else 17
                                else 17
                              else 17
                    else 17
                  else 17
                else if x <= 69478
                  then if x <= 68989
                    then if x <= 68744
                      then if x <= 68622
                        then 17
                        else if x <= 68683
                          then 17
                          else if x <= 68714
                            then 17
                            else if x <= 68729
                              then 17
                              else if x <= 68737
                                then if x <= 68733
                                  then 17
                                  else if x <= 68735
                                    then 17
                                    else 7
                                else 7
                      else if x <= 68867
                        then if x <= 68806
                          then if x <= 68775
                            then 7
                            else if x <= 68791
                              then if x <= 68783
                                then 7
                                else if x <= 68787
                                  then if x <= 68785
                                    then 7
                                    else if x <= 68786
                                      then 7
                                      else 17
                                  else 17
                              else if x <= 68799
                                then 17
                                else 7
                          else if x <= 68837
                            then 7
                            else if x <= 68852
                              then if x <= 68845
                                then 7
                                else if x <= 68849
                                  then 7
                                  else if x <= 68851
                                    then if x <= 68850
                                      then 7
                                      else 17
                                    else 17
                              else 17
                        else 17
                    else 17
                  else 17
            else if x <= 74370
              then if x <= 72413
                then if x <= 71435
                  then 17
                  else if x <= 71924
                    then if x <= 71680
                      then 17
                      else if x <= 71802
                        then 17
                        else if x <= 71863
                          then if x <= 71833
                            then 17
                            else if x <= 71848
                              then if x <= 71841
                                then if x <= 71837
                                  then 17
                                  else if x <= 71839
                                    then 17
                                    else 7
                                else 7
                              else 7
                          else if x <= 71894
                            then 7
                            else if x <= 71909
                              then if x <= 71902
                                then 7
                                else if x <= 71906
                                  then if x <= 71904
                                    then if x <= 71903
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                              else 17
                    else 17
                else 17
              else 17
          else if x <= 86113
            then 17
            else if x <= 90027
              then 17
              else if x <= 91984
                then 17
                else if x <= 92963
                  then 17
                  else if x <= 93452
                    then 17
                    else if x <= 93697
                      then 17
                      else if x <= 93819
                        then if x <= 93758
                          then 17
                          else if x <= 93789
                            then if x <= 93774
                              then if x <= 93766
                                then if x <= 93762
                                  then if x <= 93760
                                    then if x <= 93759
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else 7
                              else 7
                            else 7
                        else if x <= 93880
                          then if x <= 93850
                            then if x <= 93835
                              then if x <= 93827
                                then if x <= 93823
                                  then 7
                                  else 17
                                else 17
                              else 17
                            else 17
                          else 17
        else if x <= 109598
          then 17
          else if x <= 117426
            then 17
            else if x <= 121340
              then if x <= 119383
                then 17
                else if x <= 120362
                  then if x <= 119873
                    then if x <= 119628
                      then 17
                      else if x <= 119751
                        then 17
                        else if x <= 119812
                          then if x <= 119782
                            then 17
                            else if x <= 119797
                              then 17
                              else if x <= 119805
                                then 17
                                else if x <= 119809
                                  then if x <= 119807
                                    then 17
                                    else 7
                                  else 7
                          else 7
                    else if x <= 120118
                      then if x <= 119996
                        then if x <= 119935
                          then if x <= 119904
                            then if x <= 119889
                              then 7
                              else if x <= 119897
                                then if x <= 119893
                                  then if x <= 119891
                                    then 7
                                    else if x <= 119892
                                      then 7
                                      else 17
                                  else 7
                                else 7
                            else 7
                          else if x <= 119966
                            then if x <= 119951
                              then 7
                              else if x <= 119959
                                then 7
                                else if x <= 119963
                                  then 7
                                  else if x <= 119965
                                    then if x <= 119964
                                      then 7
                                      else 17
                                    else 7
                            else if x <= 119981
                              then if x <= 119974
                                then if x <= 119970
                                  then if x <= 119968
                                    then if x <= 119967
                                      then 7
                                      else 17
                                    else if x <= 119969
                                      then 17
                                      else 7
                                  else if x <= 119972
                                    then 17
                                    else 7
                                else if x <= 119978
                                  then if x <= 119976
                                    then 17
                                    else 7
                                  else if x <= 119980
                                    then 7
                                    else 17
                              else if x <= 119989
                                then 7
                                else if x <= 119993
                                  then 7
                                  else if x <= 119995
                                    then if x <= 119994
                                      then 17
                                      else 7
                                    else 17
                        else if x <= 120057
                          then if x <= 120027
                            then if x <= 120012
                              then if x <= 120004
                                then if x <= 120000
                                  then 7
                                  else if x <= 120002
                                    then 7
                                    else if x <= 120003
                                      then 7
                                      else 17
                                else 7
                              else 7
                            else 7
                          else if x <= 120088
                            then if x <= 120073
                              then if x <= 120065
                                then 7
                                else if x <= 120069
                                  then 7
                                  else if x <= 120071
                                    then if x <= 120070
                                      then 17
                                      else 7
                                    else 7
                              else if x <= 120081
                                then if x <= 120077
                                  then if x <= 120075
                                    then if x <= 120074
                                      then 7
                                      else 17
                                    else if x <= 120076
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 120085
                                  then if x <= 120083
                                    then 7
                                    else if x <= 120084
                                      then 7
                                      else 17
                                  else 7
                            else if x <= 120103
                              then if x <= 120096
                                then if x <= 120092
                                  then 7
                                  else if x <= 120094
                                    then if x <= 120093
                                      then 17
                                      else 7
                                    else 7
                                else 7
                              else 7
                      else if x <= 120240
                        then if x <= 120179
                          then if x <= 120149
                            then if x <= 120134
                              then if x <= 120126
                                then if x <= 120122
                                  then if x <= 120120
                                    then 7
                                    else if x <= 120121
                                      then 7
                                      else 17
                                  else 7
                                else if x <= 120130
                                  then if x <= 120128
                                    then if x <= 120127
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 120132
                                    then 7
                                    else if x <= 120133
                                      then 17
                                      else 7
                              else if x <= 120142
                                then if x <= 120138
                                  then if x <= 120136
                                    then 17
                                    else if x <= 120137
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 120146
                                  then if x <= 120144
                                    then 7
                                    else if x <= 120145
                                      then 17
                                      else 7
                                  else 7
                            else 7
                          else 7
                        else 7
                  else if x <= 120851
                    then if x <= 120607
                      then if x <= 120485
                        then 7
                        else if x <= 120546
                          then if x <= 120516
                            then if x <= 120501
                              then if x <= 120493
                                then if x <= 120489
                                  then if x <= 120487
                                    then 17
                                    else 7
                                  else 7
                                else 7
                              else if x <= 120509
                                then 7
                                else if x <= 120513
                                  then if x <= 120511
                                    then 7
                                    else if x <= 120512
                                      then 7
                                      else 17
                                  else 7
                            else if x <= 120531
                              then 7
                              else if x <= 120539
                                then if x <= 120535
                                  then 7
                                  else if x <= 120537
                                    then 7
                                    else if x <= 120538
                                      then 7
                                      else 17
                                else 7
                          else if x <= 120577
                            then if x <= 120562
                              then 7
                              else if x <= 120570
                                then 7
                                else if x <= 120574
                                  then if x <= 120572
                                    then if x <= 120571
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                            else if x <= 120592
                              then 7
                              else if x <= 120600
                                then if x <= 120596
                                  then 7
                                  else if x <= 120598
                                    then if x <= 120597
                                      then 17
                                      else 7
                                    else 7
                                else 7
                      else if x <= 120729
                        then if x <= 120668
                          then if x <= 120638
                            then if x <= 120623
                              then 7
                              else if x <= 120631
                                then if x <= 120627
                                  then 7
                                  else if x <= 120629
                                    then if x <= 120628
                                      then 7
                                      else 17
                                    else 7
                                else 7
                            else if x <= 120653
                              then 7
                              else if x <= 120661
                                then if x <= 120657
                                  then if x <= 120655
                                    then if x <= 120654
                                      then 7
                                      else 17
                                    else 7
                                  else 7
                                else 7
                          else if x <= 120699
                            then if x <= 120684
                              then 7
                              else if x <= 120692
                                then if x <= 120688
                                  then if x <= 120686
                                    then 7
                                    else if x <= 120687
                                      then 17
                                      else 7
                                  else 7
                                else 7
                            else if x <= 120714
                              then if x <= 120707
                                then 7
                                else if x <= 120711
                                  then 7
                                  else if x <= 120713
                                    then if x <= 120712
                                      then 7
                                      else 17
                                    else 7
                              else 7
                        else if x <= 120790
                          then if x <= 120760
                            then if x <= 120745
                              then if x <= 120737
                                then 7
                                else if x <= 120741
                                  then 7
                                  else if x <= 120743
                                    then 7
                                    else if x <= 120744
                                      then 7
                                      else 17
                              else 7
                            else if x <= 120775
                              then if x <= 120768
                                then 7
                                else if x <= 120772
                                  then if x <= 120770
                                    then 7
                                    else if x <= 120771
                                      then 17
                                      else 7
                                  else 7
                              else if x <= 120783
                                then if x <= 120779
                                  then 7
                                  else 17
                                else 17
                          else 17
                    else 17
              else if x <= 123297
                then if x <= 122319
                  then 17
                  else if x <= 122808
                    then if x <= 122564
                      then 17
                      else if x <= 122686
                        then if x <= 122625
                          then if x <= 122595
                            then 17
                            else if x <= 122610
                              then 17
                              else if x <= 122618
                                then 17
                                else if x <= 122622
                                  then 17
                                  else if x <= 122624
                                    then if x <= 122623
                                      then 17
                                      else 7
                                    else 7
                          else if x <= 122656
                            then if x <= 122641
                              then if x <= 122633
                                then 7
                                else if x <= 122637
                                  then if x <= 122635
                                    then if x <= 122634
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                              else if x <= 122649
                                then 7
                                else if x <= 122653
                                  then 7
                                  else if x <= 122655
                                    then if x <= 122654
                                      then 7
                                      else 17
                                    else 17
                            else if x <= 122671
                              then if x <= 122664
                                then if x <= 122660
                                  then 17
                                  else 7
                                else if x <= 122668
                                  then if x <= 122666
                                    then 7
                                    else 17
                                  else 17
                              else 17
                        else 17
                    else 17
                else if x <= 124276
                  then 17
                  else if x <= 124765
                    then 17
                    else if x <= 125010
                      then 17
                      else if x <= 125132
                        then 17
                        else if x <= 125193
                          then if x <= 125163
                            then 17
                            else if x <= 125178
                              then 17
                              else if x <= 125186
                                then if x <= 125182
                                  then 17
                                  else if x <= 125184
                                    then if x <= 125183
                                      then 17
                                      else 7
                                    else 7
                                else 7
                          else if x <= 125224
                            then 7
                            else if x <= 125239
                              then 7
                              else if x <= 125247
                                then 7
                                else if x <= 125251
                                  then 7
                                  else 17
    else 17
discriminatorTrans 8 x =
  if x <= -1
  then -1
  else if x <= 125254
    then 17
    else 17
discriminatorTrans 9 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then if x <= 15
                              then if x <= 7
                                then 9
                                else if x <= 11
                                  then if x <= 9
                                    then 9
                                    else if x <= 10
                                      then 17
                                      else 9
                                  else 9
                              else 9
                            else 9
                          else 9
                        else 9
                      else 9
                    else 9
                  else 9
                else 9
              else 9
            else 9
          else 9
        else 9
      else 9
    else 9
discriminatorTrans 10 x =
  if x <= -1
  then -1
  else if x <= 125254
    then 17
    else 17
discriminatorTrans 11 x =
  if x <= -1
  then -1
  else if x <= 125254
    then 17
    else 17
discriminatorTrans 12 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then if x <= 15
                              then if x <= 7
                                then 17
                                else if x <= 11
                                  then if x <= 9
                                    then if x <= 8
                                      then 17
                                      else 0
                                    else 0
                                  else if x <= 13
                                    then 0
                                    else 17
                              else 17
                            else if x <= 46
                              then if x <= 38
                                then if x <= 34
                                  then if x <= 32
                                    then if x <= 31
                                      then 17
                                      else 0
                                    else if x <= 33
                                      then 17
                                      else 13
                                  else 17
                                else if x <= 42
                                  then if x <= 40
                                    then if x <= 39
                                      then 17
                                      else 4
                                    else if x <= 41
                                      then 5
                                      else 17
                                  else if x <= 44
                                    then 17
                                    else if x <= 45
                                      then 17
                                      else 7
                              else if x <= 54
                                then if x <= 50
                                  then if x <= 48
                                    then if x <= 47
                                      then 15
                                      else 17
                                    else 17
                                  else 17
                                else if x <= 58
                                  then 17
                                  else if x <= 60
                                    then if x <= 59
                                      then 3
                                      else 17
                                    else 8
                          else if x <= 92
                            then if x <= 77
                              then if x <= 69
                                then if x <= 65
                                  then if x <= 63
                                    then 17
                                    else if x <= 64
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else if x <= 85
                                then 7
                                else if x <= 89
                                  then 7
                                  else if x <= 91
                                    then if x <= 90
                                      then 7
                                      else 10
                                    else 17
                            else if x <= 107
                              then if x <= 100
                                then if x <= 96
                                  then if x <= 94
                                    then if x <= 93
                                      then 11
                                      else 17
                                    else 17
                                  else 7
                                else 7
                              else 7
                        else if x <= 183
                          then if x <= 153
                            then 17
                            else if x <= 168
                              then if x <= 161
                                then if x <= 157
                                  then 17
                                  else if x <= 159
                                    then 17
                                    else if x <= 160
                                      then 0
                                      else 17
                                else 17
                              else if x <= 176
                                then 17
                                else if x <= 180
                                  then 17
                                  else if x <= 182
                                    then if x <= 181
                                      then 7
                                      else 17
                                    else 17
                          else if x <= 214
                            then if x <= 199
                              then if x <= 191
                                then 17
                                else 7
                              else 7
                            else if x <= 229
                              then if x <= 222
                                then if x <= 218
                                  then if x <= 216
                                    then if x <= 215
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else 7
                              else 7
                      else if x <= 367
                        then if x <= 306
                          then if x <= 275
                            then if x <= 260
                              then if x <= 252
                                then if x <= 248
                                  then if x <= 246
                                    then 7
                                    else if x <= 247
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else 7
                            else 7
                          else 7
                        else if x <= 428
                          then 7
                          else if x <= 459
                            then if x <= 444
                              then if x <= 436
                                then 7
                                else if x <= 440
                                  then 7
                                  else if x <= 442
                                    then 7
                                    else if x <= 443
                                      then 17
                                      else 7
                              else if x <= 452
                                then if x <= 448
                                  then if x <= 446
                                    then 7
                                    else if x <= 447
                                      then 7
                                      else 17
                                  else if x <= 450
                                    then 17
                                    else if x <= 451
                                      then 17
                                      else 7
                                else if x <= 456
                                  then if x <= 454
                                    then if x <= 453
                                      then 17
                                      else 7
                                    else if x <= 455
                                      then 7
                                      else 17
                                  else if x <= 458
                                    then 7
                                    else 17
                            else 7
                    else if x <= 734
                      then if x <= 612
                        then if x <= 551
                          then if x <= 520
                            then if x <= 505
                              then if x <= 497
                                then 7
                                else if x <= 501
                                  then if x <= 499
                                    then if x <= 498
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                              else 7
                            else 7
                          else 7
                        else if x <= 673
                          then if x <= 643
                            then 7
                            else if x <= 658
                              then 7
                              else if x <= 666
                                then if x <= 662
                                  then if x <= 660
                                    then if x <= 659
                                      then 7
                                      else 17
                                    else 7
                                  else 7
                                else 7
                          else if x <= 704
                            then if x <= 689
                              then if x <= 681
                                then 7
                                else if x <= 685
                                  then 7
                                  else if x <= 687
                                    then 7
                                    else 17
                              else 17
                            else 17
                      else if x <= 856
                        then 17
                        else if x <= 917
                          then if x <= 887
                            then if x <= 872
                              then 17
                              else if x <= 880
                                then if x <= 876
                                  then 17
                                  else if x <= 878
                                    then 17
                                    else if x <= 879
                                      then 17
                                      else 7
                                else if x <= 884
                                  then if x <= 882
                                    then 7
                                    else if x <= 883
                                      then 7
                                      else 17
                                  else if x <= 886
                                    then if x <= 885
                                      then 17
                                      else 7
                                    else 7
                            else if x <= 902
                              then if x <= 895
                                then if x <= 891
                                  then if x <= 889
                                    then 17
                                    else if x <= 890
                                      then 17
                                      else 7
                                  else if x <= 893
                                    then 7
                                    else if x <= 894
                                      then 17
                                      else 7
                                else if x <= 899
                                  then 17
                                  else if x <= 901
                                    then 17
                                    else 7
                              else if x <= 910
                                then if x <= 906
                                  then if x <= 904
                                    then if x <= 903
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 908
                                    then if x <= 907
                                      then 17
                                      else 7
                                    else if x <= 909
                                      then 17
                                      else 7
                                else 7
                          else if x <= 948
                            then if x <= 933
                              then if x <= 925
                                then 7
                                else if x <= 929
                                  then 7
                                  else if x <= 931
                                    then if x <= 930
                                      then 17
                                      else 7
                                    else 7
                              else 7
                            else 7
                  else if x <= 1468
                    then if x <= 1223
                      then if x <= 1101
                        then if x <= 1040
                          then if x <= 1009
                            then 7
                            else if x <= 1025
                              then if x <= 1017
                                then if x <= 1013
                                  then 7
                                  else if x <= 1015
                                    then if x <= 1014
                                      then 17
                                      else 7
                                    else 7
                                else 7
                              else 7
                          else 7
                        else if x <= 1162
                          then if x <= 1132
                            then 7
                            else if x <= 1147
                              then 7
                              else if x <= 1155
                                then if x <= 1151
                                  then 7
                                  else if x <= 1153
                                    then 7
                                    else 17
                                else if x <= 1159
                                  then 17
                                  else if x <= 1161
                                    then 17
                                    else 7
                          else 7
                      else if x <= 1346
                        then if x <= 1285
                          then 7
                          else if x <= 1316
                            then 7
                            else if x <= 1331
                              then if x <= 1324
                                then 7
                                else if x <= 1328
                                  then if x <= 1326
                                    then 7
                                    else if x <= 1327
                                      then 7
                                      else 17
                                  else 7
                              else 7
                        else if x <= 1407
                          then if x <= 1377
                            then if x <= 1362
                              then 7
                              else if x <= 1370
                                then if x <= 1366
                                  then 7
                                  else 17
                                else if x <= 1374
                                  then 17
                                  else if x <= 1376
                                    then if x <= 1375
                                      then 17
                                      else 7
                                    else 7
                            else 7
                          else if x <= 1438
                            then if x <= 1423
                              then if x <= 1415
                                then 7
                                else if x <= 1419
                                  then if x <= 1417
                                    then if x <= 1416
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                              else 17
                            else 17
                    else 17
                else 17
              else if x <= 5871
                then if x <= 4893
                  then if x <= 4404
                    then if x <= 4159
                      then 17
                      else if x <= 4282
                        then if x <= 4221
                          then 17
                          else if x <= 4252
                            then 17
                            else if x <= 4267
                              then if x <= 4260
                                then if x <= 4256
                                  then if x <= 4254
                                    then 17
                                    else if x <= 4255
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else 7
                        else if x <= 4343
                          then if x <= 4313
                            then if x <= 4298
                              then if x <= 4290
                                then 7
                                else if x <= 4294
                                  then if x <= 4292
                                    then 7
                                    else if x <= 4293
                                      then 7
                                      else 17
                                  else if x <= 4296
                                    then if x <= 4295
                                      then 7
                                      else 17
                                    else 17
                              else if x <= 4306
                                then if x <= 4302
                                  then if x <= 4300
                                    then 17
                                    else if x <= 4301
                                      then 7
                                      else 17
                                  else if x <= 4304
                                    then if x <= 4303
                                      then 17
                                      else 7
                                    else 7
                                else 7
                            else 7
                          else if x <= 4374
                            then if x <= 4359
                              then if x <= 4351
                                then if x <= 4347
                                  then if x <= 4345
                                    then 7
                                    else if x <= 4346
                                      then 7
                                      else 17
                                  else if x <= 4349
                                    then if x <= 4348
                                      then 17
                                      else 7
                                    else 7
                                else 17
                              else 17
                            else 17
                    else 17
                  else if x <= 5382
                    then if x <= 5138
                      then if x <= 5016
                        then 17
                        else if x <= 5077
                          then if x <= 5047
                            then if x <= 5032
                              then if x <= 5024
                                then if x <= 5020
                                  then 17
                                  else if x <= 5022
                                    then 17
                                    else if x <= 5023
                                      then 17
                                      else 7
                                else 7
                              else 7
                            else 7
                          else if x <= 5108
                            then 7
                            else if x <= 5123
                              then if x <= 5116
                                then if x <= 5112
                                  then if x <= 5110
                                    then if x <= 5109
                                      then 7
                                      else 17
                                    else if x <= 5111
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 5120
                                  then if x <= 5118
                                    then if x <= 5117
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                              else 17
                      else 17
                    else if x <= 5627
                      then 17
                      else if x <= 5749
                        then 17
                        else if x <= 5810
                          then if x <= 5780
                            then if x <= 5765
                              then if x <= 5757
                                then 17
                                else if x <= 5761
                                  then if x <= 5759
                                    then 17
                                    else if x <= 5760
                                      then 0
                                      else 17
                                  else 17
                              else 17
                            else 17
                          else 17
                else if x <= 6850
                  then 17
                  else if x <= 7339
                    then if x <= 7095
                      then 17
                      else if x <= 7217
                        then 17
                        else if x <= 7278
                          then 17
                          else if x <= 7309
                            then if x <= 7294
                              then 17
                              else if x <= 7302
                                then if x <= 7298
                                  then if x <= 7296
                                    then if x <= 7295
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else if x <= 7306
                                  then if x <= 7304
                                    then 7
                                    else 17
                                  else 17
                            else if x <= 7324
                              then if x <= 7317
                                then if x <= 7313
                                  then if x <= 7311
                                    then 17
                                    else 7
                                  else 7
                                else 7
                              else 7
                    else if x <= 7584
                      then if x <= 7462
                        then if x <= 7401
                          then if x <= 7370
                            then if x <= 7355
                              then if x <= 7347
                                then 7
                                else if x <= 7351
                                  then 7
                                  else if x <= 7353
                                    then 7
                                    else if x <= 7354
                                      then 7
                                      else 17
                              else if x <= 7363
                                then if x <= 7359
                                  then if x <= 7357
                                    then if x <= 7356
                                      then 17
                                      else 7
                                    else 7
                                  else 17
                                else 17
                            else 17
                          else if x <= 7432
                            then if x <= 7417
                              then 17
                              else if x <= 7425
                                then if x <= 7421
                                  then 17
                                  else if x <= 7423
                                    then 17
                                    else 7
                                else 7
                            else 7
                        else if x <= 7523
                          then if x <= 7493
                            then if x <= 7478
                              then if x <= 7470
                                then if x <= 7466
                                  then 7
                                  else if x <= 7468
                                    then if x <= 7467
                                      then 7
                                      else 17
                                    else 17
                                else 17
                              else 17
                            else 17
                          else if x <= 7554
                            then if x <= 7539
                              then if x <= 7531
                                then if x <= 7527
                                  then 17
                                  else if x <= 7529
                                    then 17
                                    else if x <= 7530
                                      then 17
                                      else 7
                                else 7
                              else if x <= 7547
                                then if x <= 7543
                                  then 7
                                  else if x <= 7545
                                    then if x <= 7544
                                      then 17
                                      else 7
                                    else 7
                                else 7
                            else if x <= 7569
                              then 7
                              else if x <= 7577
                                then 7
                                else if x <= 7581
                                  then if x <= 7579
                                    then if x <= 7578
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                      else if x <= 7706
                        then if x <= 7645
                          then 17
                          else if x <= 7676
                            then 17
                            else if x <= 7691
                              then if x <= 7684
                                then if x <= 7680
                                  then if x <= 7678
                                    then 17
                                    else if x <= 7679
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else 7
                        else 7
            else if x <= 11742
              then if x <= 9785
                then if x <= 8807
                  then if x <= 8318
                    then if x <= 8073
                      then if x <= 7951
                        then 7
                        else if x <= 8012
                          then if x <= 7982
                            then if x <= 7967
                              then if x <= 7959
                                then if x <= 7955
                                  then 7
                                  else if x <= 7957
                                    then 7
                                    else 17
                                else if x <= 7963
                                  then 7
                                  else if x <= 7965
                                    then 7
                                    else 17
                              else 7
                            else if x <= 7997
                              then 7
                              else if x <= 8005
                                then 7
                                else if x <= 8009
                                  then if x <= 8007
                                    then 17
                                    else 7
                                  else 7
                          else if x <= 8043
                            then if x <= 8028
                              then if x <= 8020
                                then if x <= 8016
                                  then if x <= 8014
                                    then if x <= 8013
                                      then 7
                                      else 17
                                    else if x <= 8015
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 8024
                                  then if x <= 8022
                                    then 7
                                    else if x <= 8023
                                      then 7
                                      else 17
                                  else if x <= 8026
                                    then if x <= 8025
                                      then 7
                                      else 17
                                    else if x <= 8027
                                      then 7
                                      else 17
                              else if x <= 8036
                                then if x <= 8032
                                  then if x <= 8030
                                    then if x <= 8029
                                      then 7
                                      else 17
                                    else 7
                                  else 7
                                else 7
                            else if x <= 8058
                              then 7
                              else if x <= 8066
                                then if x <= 8062
                                  then if x <= 8060
                                    then 7
                                    else if x <= 8061
                                      then 7
                                      else 17
                                  else if x <= 8064
                                    then if x <= 8063
                                      then 17
                                      else 7
                                    else 7
                                else if x <= 8070
                                  then 7
                                  else if x <= 8072
                                    then if x <= 8071
                                      then 7
                                      else 17
                                    else 17
                      else if x <= 8196
                        then if x <= 8135
                          then if x <= 8104
                            then if x <= 8089
                              then if x <= 8081
                                then if x <= 8077
                                  then 17
                                  else if x <= 8079
                                    then 17
                                    else 7
                                else if x <= 8085
                                  then 7
                                  else if x <= 8087
                                    then 7
                                    else 17
                              else if x <= 8097
                                then if x <= 8093
                                  then 17
                                  else if x <= 8095
                                    then 17
                                    else 7
                                else if x <= 8101
                                  then 7
                                  else if x <= 8103
                                    then 7
                                    else 17
                            else if x <= 8120
                              then if x <= 8112
                                then if x <= 8108
                                  then 17
                                  else if x <= 8110
                                    then 17
                                    else if x <= 8111
                                      then 17
                                      else 7
                                else if x <= 8116
                                  then 7
                                  else if x <= 8118
                                    then if x <= 8117
                                      then 17
                                      else 7
                                    else 7
                              else if x <= 8128
                                then if x <= 8124
                                  then if x <= 8122
                                    then 7
                                    else if x <= 8123
                                      then 7
                                      else 17
                                  else if x <= 8126
                                    then if x <= 8125
                                      then 17
                                      else 7
                                    else 17
                                else if x <= 8132
                                  then if x <= 8130
                                    then if x <= 8129
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 8134
                                    then if x <= 8133
                                      then 17
                                      else 7
                                    else 7
                          else if x <= 8166
                            then if x <= 8151
                              then if x <= 8143
                                then if x <= 8139
                                  then 7
                                  else 17
                                else if x <= 8147
                                  then 7
                                  else if x <= 8149
                                    then 17
                                    else 7
                              else if x <= 8159
                                then if x <= 8155
                                  then 7
                                  else 17
                                else 7
                            else if x <= 8181
                              then if x <= 8174
                                then if x <= 8170
                                  then 7
                                  else if x <= 8172
                                    then 7
                                    else 17
                                else if x <= 8178
                                  then if x <= 8176
                                    then 17
                                    else if x <= 8177
                                      then 17
                                      else 7
                                  else if x <= 8180
                                    then 7
                                    else 17
                              else if x <= 8189
                                then if x <= 8185
                                  then 7
                                  else if x <= 8187
                                    then 7
                                    else 17
                                else if x <= 8193
                                  then if x <= 8191
                                    then 17
                                    else 0
                                  else 0
                        else if x <= 8257
                          then if x <= 8227
                            then if x <= 8212
                              then if x <= 8204
                                then if x <= 8200
                                  then 0
                                  else if x <= 8202
                                    then 0
                                    else 17
                                else 17
                              else 17
                            else if x <= 8242
                              then if x <= 8235
                                then 17
                                else if x <= 8239
                                  then if x <= 8237
                                    then 17
                                    else if x <= 8238
                                      then 17
                                      else 0
                                  else 17
                              else 17
                          else if x <= 8288
                            then if x <= 8273
                              then 17
                              else if x <= 8281
                                then 17
                                else if x <= 8285
                                  then 17
                                  else if x <= 8287
                                    then if x <= 8286
                                      then 17
                                      else 0
                                    else 17
                            else 17
                    else if x <= 8563
                      then if x <= 8441
                        then 17
                        else if x <= 8502
                          then if x <= 8472
                            then if x <= 8457
                              then if x <= 8449
                                then 17
                                else if x <= 8453
                                  then if x <= 8451
                                    then if x <= 8450
                                      then 7
                                      else 17
                                    else 17
                                  else if x <= 8455
                                    then if x <= 8454
                                      then 17
                                      else 7
                                    else 17
                              else if x <= 8465
                                then 7
                                else if x <= 8469
                                  then if x <= 8467
                                    then 7
                                    else if x <= 8468
                                      then 17
                                      else 7
                                  else 17
                            else if x <= 8487
                              then if x <= 8480
                                then if x <= 8476
                                  then 7
                                  else if x <= 8478
                                    then if x <= 8477
                                      then 7
                                      else 17
                                    else 17
                                else if x <= 8484
                                  then if x <= 8482
                                    then 17
                                    else if x <= 8483
                                      then 17
                                      else 7
                                  else if x <= 8486
                                    then if x <= 8485
                                      then 17
                                      else 7
                                    else 17
                              else if x <= 8495
                                then if x <= 8491
                                  then if x <= 8489
                                    then if x <= 8488
                                      then 7
                                      else 17
                                    else 7
                                  else if x <= 8493
                                    then 7
                                    else if x <= 8494
                                      then 17
                                      else 7
                                else if x <= 8499
                                  then 7
                                  else if x <= 8501
                                    then if x <= 8500
                                      then 7
                                      else 17
                                    else 17
                          else if x <= 8533
                            then if x <= 8518
                              then if x <= 8510
                                then if x <= 8506
                                  then if x <= 8504
                                    then 17
                                    else if x <= 8505
                                      then 7
                                      else 17
                                  else if x <= 8508
                                    then if x <= 8507
                                      then 17
                                      else 7
                                    else 7
                                else if x <= 8514
                                  then if x <= 8512
                                    then if x <= 8511
                                      then 7
                                      else 17
                                    else 17
                                  else if x <= 8516
                                    then 17
                                    else 7
                              else if x <= 8526
                                then if x <= 8522
                                  then if x <= 8520
                                    then 7
                                    else if x <= 8521
                                      then 7
                                      else 17
                                  else if x <= 8524
                                    then 17
                                    else if x <= 8525
                                      then 17
                                      else 7
                                else 17
                            else 17
                      else if x <= 8685
                        then if x <= 8624
                          then if x <= 8594
                            then if x <= 8579
                              then if x <= 8571
                                then 17
                                else if x <= 8575
                                  then 17
                                  else if x <= 8577
                                    then 17
                                    else if x <= 8578
                                      then 17
                                      else 7
                              else if x <= 8587
                                then if x <= 8583
                                  then if x <= 8581
                                    then if x <= 8580
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                                else 17
                            else 17
                          else 17
                        else 17
                  else 17
                else if x <= 10764
                  then 17
                  else if x <= 11253
                    then 17
                    else if x <= 11498
                      then if x <= 11376
                        then if x <= 11315
                          then if x <= 11284
                            then if x <= 11269
                              then if x <= 11261
                                then 17
                                else if x <= 11265
                                  then if x <= 11263
                                    then 17
                                    else 7
                                  else 7
                              else 7
                            else 7
                          else 7
                        else if x <= 11437
                          then if x <= 11407
                            then if x <= 11392
                              then if x <= 11384
                                then 7
                                else if x <= 11388
                                  then if x <= 11386
                                    then 7
                                    else if x <= 11387
                                      then 7
                                      else 17
                                  else if x <= 11390
                                    then if x <= 11389
                                      then 17
                                      else 7
                                    else 7
                              else 7
                            else 7
                          else if x <= 11468
                            then 7
                            else if x <= 11483
                              then 7
                              else if x <= 11491
                                then 7
                                else if x <= 11495
                                  then if x <= 11493
                                    then if x <= 11492
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                      else if x <= 11620
                        then if x <= 11559
                          then if x <= 11529
                            then if x <= 11514
                              then if x <= 11506
                                then if x <= 11502
                                  then 7
                                  else if x <= 11504
                                    then 17
                                    else if x <= 11505
                                      then 17
                                      else 7
                                else if x <= 11510
                                  then if x <= 11508
                                    then if x <= 11507
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                              else if x <= 11522
                                then if x <= 11518
                                  then 17
                                  else if x <= 11520
                                    then if x <= 11519
                                      then 17
                                      else 7
                                    else 7
                                else 7
                            else if x <= 11544
                              then 7
                              else if x <= 11552
                                then 7
                                else if x <= 11556
                                  then 7
                                  else if x <= 11558
                                    then if x <= 11557
                                      then 7
                                      else 17
                                    else 7
                          else if x <= 11590
                            then if x <= 11575
                              then if x <= 11567
                                then if x <= 11563
                                  then 17
                                  else if x <= 11565
                                    then if x <= 11564
                                      then 17
                                      else 7
                                    else 17
                                else 17
                              else 17
                            else 17
                        else 17
              else if x <= 13699
                then if x <= 12721
                  then if x <= 12232
                    then 17
                    else if x <= 12477
                      then if x <= 12355
                        then if x <= 12294
                          then if x <= 12263
                            then 17
                            else if x <= 12279
                              then 17
                              else if x <= 12287
                                then 17
                                else if x <= 12291
                                  then if x <= 12289
                                    then if x <= 12288
                                      then 0
                                      else 17
                                    else 17
                                  else 17
                          else 17
                        else 17
                      else 17
                  else 17
                else 17
          else 17
        else if x <= 46970
          then if x <= 39142
            then 17
            else if x <= 43056
              then if x <= 41099
                then 17
                else if x <= 42078
                  then 17
                  else if x <= 42567
                    then if x <= 42323
                      then 17
                      else if x <= 42445
                        then 17
                        else if x <= 42506
                          then 17
                          else if x <= 42537
                            then 17
                            else if x <= 42552
                              then 17
                              else if x <= 42560
                                then if x <= 42556
                                  then 17
                                  else if x <= 42558
                                    then 17
                                    else if x <= 42559
                                      then 17
                                      else 7
                                else 7
                    else if x <= 42812
                      then if x <= 42690
                        then if x <= 42629
                          then if x <= 42598
                            then 7
                            else if x <= 42614
                              then if x <= 42606
                                then if x <= 42602
                                  then 7
                                  else if x <= 42604
                                    then 7
                                    else if x <= 42605
                                      then 7
                                      else 17
                                else 17
                              else if x <= 42622
                                then 17
                                else if x <= 42626
                                  then if x <= 42624
                                    then if x <= 42623
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                          else if x <= 42660
                            then if x <= 42645
                              then 7
                              else if x <= 42653
                                then if x <= 42649
                                  then 7
                                  else if x <= 42651
                                    then 7
                                    else 17
                                else 17
                            else 17
                        else if x <= 42751
                          then 17
                          else if x <= 42782
                            then 17
                            else if x <= 42797
                              then if x <= 42790
                                then if x <= 42786
                                  then if x <= 42784
                                    then 17
                                    else if x <= 42785
                                      then 17
                                      else 7
                                  else 7
                                else 7
                              else 7
                      else if x <= 42934
                        then if x <= 42873
                          then if x <= 42843
                            then 7
                            else if x <= 42858
                              then 7
                              else if x <= 42866
                                then if x <= 42862
                                  then 7
                                  else if x <= 42864
                                    then if x <= 42863
                                      then 7
                                      else 17
                                    else 7
                                else 7
                          else if x <= 42904
                            then if x <= 42889
                              then if x <= 42881
                                then 7
                                else if x <= 42885
                                  then 7
                                  else if x <= 42887
                                    then 7
                                    else 17
                              else if x <= 42897
                                then if x <= 42893
                                  then if x <= 42891
                                    then if x <= 42890
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 42895
                                    then if x <= 42894
                                      then 7
                                      else 17
                                    else 7
                                else 7
                            else 7
                        else if x <= 42995
                          then if x <= 42965
                            then if x <= 42950
                              then 7
                              else if x <= 42958
                                then if x <= 42954
                                  then 7
                                  else 17
                                else if x <= 42962
                                  then if x <= 42960
                                    then if x <= 42959
                                      then 17
                                      else 7
                                    else if x <= 42961
                                      then 7
                                      else 17
                                  else if x <= 42964
                                    then if x <= 42963
                                      then 7
                                      else 17
                                    else 7
                            else if x <= 42980
                              then if x <= 42973
                                then if x <= 42969
                                  then 7
                                  else 17
                                else 17
                              else 17
                          else if x <= 43026
                            then if x <= 43011
                              then if x <= 43003
                                then if x <= 42999
                                  then if x <= 42997
                                    then if x <= 42996
                                      then 17
                                      else 7
                                    else if x <= 42998
                                      then 7
                                      else 17
                                  else if x <= 43001
                                    then 17
                                    else if x <= 43002
                                      then 7
                                      else 17
                                else 17
                              else 17
                            else 17
              else if x <= 45013
                then if x <= 44035
                  then if x <= 43546
                    then 17
                    else if x <= 43791
                      then 17
                      else if x <= 43913
                        then if x <= 43852
                          then if x <= 43822
                            then 17
                            else if x <= 43837
                              then if x <= 43830
                                then if x <= 43826
                                  then if x <= 43824
                                    then if x <= 43823
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else 7
                              else 7
                          else if x <= 43883
                            then if x <= 43868
                              then if x <= 43860
                                then 7
                                else if x <= 43864
                                  then 7
                                  else if x <= 43866
                                    then 7
                                    else 17
                              else if x <= 43876
                                then if x <= 43872
                                  then if x <= 43870
                                    then 17
                                    else if x <= 43871
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 43880
                                  then 7
                                  else 17
                            else if x <= 43898
                              then if x <= 43891
                                then if x <= 43887
                                  then 17
                                  else 7
                                else 7
                              else 7
                        else if x <= 43974
                          then if x <= 43944
                            then 7
                            else if x <= 43959
                              then 7
                              else if x <= 43967
                                then 7
                                else 17
                          else 17
                  else 17
                else 17
          else 17
      else if x <= 93941
        then if x <= 78284
          then if x <= 70456
            then if x <= 66542
              then if x <= 64585
                then if x <= 63606
                  then 17
                  else if x <= 64096
                    then 17
                    else if x <= 64341
                      then if x <= 64219
                        then 17
                        else if x <= 64280
                          then if x <= 64250
                            then 17
                            else if x <= 64265
                              then if x <= 64258
                                then if x <= 64254
                                  then 17
                                  else if x <= 64256
                                    then if x <= 64255
                                      then 17
                                      else 7
                                    else 7
                                else if x <= 64262
                                  then 7
                                  else 17
                              else if x <= 64273
                                then 17
                                else if x <= 64277
                                  then if x <= 64275
                                    then if x <= 64274
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 64279
                                    then 7
                                    else 17
                          else 17
                      else 17
                else if x <= 65564
                  then if x <= 65075
                    then 17
                    else if x <= 65320
                      then if x <= 65198
                        then 17
                        else if x <= 65259
                          then 17
                          else if x <= 65290
                            then 17
                            else if x <= 65305
                              then 17
                              else if x <= 65313
                                then if x <= 65309
                                  then 17
                                  else if x <= 65311
                                    then 17
                                    else if x <= 65312
                                      then 17
                                      else 7
                                else 7
                      else if x <= 65442
                        then if x <= 65381
                          then if x <= 65351
                            then if x <= 65336
                              then 7
                              else if x <= 65344
                                then if x <= 65340
                                  then if x <= 65338
                                    then 7
                                    else 17
                                  else 17
                                else 7
                            else if x <= 65366
                              then 7
                              else if x <= 65374
                                then if x <= 65370
                                  then 7
                                  else 17
                                else 17
                          else 17
                        else 17
                  else 17
              else if x <= 68499
                then if x <= 67521
                  then if x <= 67032
                    then if x <= 66787
                      then if x <= 66665
                        then if x <= 66604
                          then if x <= 66573
                            then if x <= 66558
                              then 17
                              else if x <= 66566
                                then if x <= 66562
                                  then if x <= 66560
                                    then if x <= 66559
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else 7
                            else 7
                          else if x <= 66635
                            then 7
                            else if x <= 66650
                              then if x <= 66643
                                then if x <= 66639
                                  then 7
                                  else 17
                                else 17
                              else 17
                        else if x <= 66726
                          then 17
                          else if x <= 66757
                            then if x <= 66742
                              then if x <= 66734
                                then 17
                                else if x <= 66738
                                  then if x <= 66736
                                    then if x <= 66735
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                              else 7
                            else if x <= 66772
                              then if x <= 66765
                                then 7
                                else if x <= 66769
                                  then 7
                                  else if x <= 66771
                                    then 7
                                    else 17
                              else if x <= 66780
                                then if x <= 66776
                                  then if x <= 66774
                                    then 17
                                    else if x <= 66775
                                      then 17
                                      else 7
                                  else 7
                                else 7
                      else if x <= 66910
                        then if x <= 66849
                          then if x <= 66818
                            then if x <= 66803
                              then 7
                              else if x <= 66811
                                then 7
                                else 17
                            else 17
                          else 17
                        else if x <= 66971
                          then if x <= 66941
                            then if x <= 66926
                              then 17
                              else if x <= 66934
                                then if x <= 66930
                                  then if x <= 66928
                                    then if x <= 66927
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else if x <= 66938
                                  then 7
                                  else if x <= 66940
                                    then if x <= 66939
                                      then 17
                                      else 7
                                    else 7
                            else if x <= 66956
                              then if x <= 66949
                                then 7
                                else if x <= 66953
                                  then 7
                                  else if x <= 66955
                                    then if x <= 66954
                                      then 7
                                      else 17
                                    else 7
                              else if x <= 66964
                                then if x <= 66960
                                  then 7
                                  else if x <= 66962
                                    then 7
                                    else if x <= 66963
                                      then 17
                                      else 7
                                else if x <= 66968
                                  then if x <= 66966
                                    then if x <= 66965
                                      then 7
                                      else 17
                                    else 7
                                  else 7
                          else if x <= 67002
                            then if x <= 66987
                              then if x <= 66979
                                then if x <= 66975
                                  then 7
                                  else if x <= 66977
                                    then 7
                                    else if x <= 66978
                                      then 17
                                      else 7
                                else 7
                              else if x <= 66995
                                then if x <= 66991
                                  then 7
                                  else if x <= 66993
                                    then 7
                                    else if x <= 66994
                                      then 17
                                      else 7
                                else if x <= 66999
                                  then 7
                                  else if x <= 67001
                                    then 7
                                    else 17
                            else if x <= 67017
                              then if x <= 67010
                                then if x <= 67006
                                  then if x <= 67004
                                    then 7
                                    else 17
                                  else 17
                                else 17
                              else 17
                    else 17
                  else 17
                else if x <= 69478
                  then if x <= 68989
                    then if x <= 68744
                      then if x <= 68622
                        then 17
                        else if x <= 68683
                          then 17
                          else if x <= 68714
                            then 17
                            else if x <= 68729
                              then 17
                              else if x <= 68737
                                then if x <= 68733
                                  then 17
                                  else if x <= 68735
                                    then 17
                                    else 7
                                else 7
                      else if x <= 68867
                        then if x <= 68806
                          then if x <= 68775
                            then 7
                            else if x <= 68791
                              then if x <= 68783
                                then 7
                                else if x <= 68787
                                  then if x <= 68785
                                    then 7
                                    else if x <= 68786
                                      then 7
                                      else 17
                                  else 17
                              else if x <= 68799
                                then 17
                                else 7
                          else if x <= 68837
                            then 7
                            else if x <= 68852
                              then if x <= 68845
                                then 7
                                else if x <= 68849
                                  then 7
                                  else if x <= 68851
                                    then if x <= 68850
                                      then 7
                                      else 17
                                    else 17
                              else 17
                        else 17
                    else 17
                  else 17
            else if x <= 74370
              then if x <= 72413
                then if x <= 71435
                  then 17
                  else if x <= 71924
                    then if x <= 71680
                      then 17
                      else if x <= 71802
                        then 17
                        else if x <= 71863
                          then if x <= 71833
                            then 17
                            else if x <= 71848
                              then if x <= 71841
                                then if x <= 71837
                                  then 17
                                  else if x <= 71839
                                    then 17
                                    else 7
                                else 7
                              else 7
                          else if x <= 71894
                            then 7
                            else if x <= 71909
                              then if x <= 71902
                                then 7
                                else if x <= 71906
                                  then if x <= 71904
                                    then if x <= 71903
                                      then 7
                                      else 17
                                    else 17
                                  else 17
                              else 17
                    else 17
                else 17
              else 17
          else if x <= 86113
            then 17
            else if x <= 90027
              then 17
              else if x <= 91984
                then 17
                else if x <= 92963
                  then 17
                  else if x <= 93452
                    then 17
                    else if x <= 93697
                      then 17
                      else if x <= 93819
                        then if x <= 93758
                          then 17
                          else if x <= 93789
                            then if x <= 93774
                              then if x <= 93766
                                then if x <= 93762
                                  then if x <= 93760
                                    then if x <= 93759
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                                else 7
                              else 7
                            else 7
                        else if x <= 93880
                          then if x <= 93850
                            then if x <= 93835
                              then if x <= 93827
                                then if x <= 93823
                                  then 7
                                  else 17
                                else 17
                              else 17
                            else 17
                          else 17
        else if x <= 109598
          then 17
          else if x <= 117426
            then 17
            else if x <= 121340
              then if x <= 119383
                then 17
                else if x <= 120362
                  then if x <= 119873
                    then if x <= 119628
                      then 17
                      else if x <= 119751
                        then 17
                        else if x <= 119812
                          then if x <= 119782
                            then 17
                            else if x <= 119797
                              then 17
                              else if x <= 119805
                                then 17
                                else if x <= 119809
                                  then if x <= 119807
                                    then 17
                                    else 7
                                  else 7
                          else 7
                    else if x <= 120118
                      then if x <= 119996
                        then if x <= 119935
                          then if x <= 119904
                            then if x <= 119889
                              then 7
                              else if x <= 119897
                                then if x <= 119893
                                  then if x <= 119891
                                    then 7
                                    else if x <= 119892
                                      then 7
                                      else 17
                                  else 7
                                else 7
                            else 7
                          else if x <= 119966
                            then if x <= 119951
                              then 7
                              else if x <= 119959
                                then 7
                                else if x <= 119963
                                  then 7
                                  else if x <= 119965
                                    then if x <= 119964
                                      then 7
                                      else 17
                                    else 7
                            else if x <= 119981
                              then if x <= 119974
                                then if x <= 119970
                                  then if x <= 119968
                                    then if x <= 119967
                                      then 7
                                      else 17
                                    else if x <= 119969
                                      then 17
                                      else 7
                                  else if x <= 119972
                                    then 17
                                    else 7
                                else if x <= 119978
                                  then if x <= 119976
                                    then 17
                                    else 7
                                  else if x <= 119980
                                    then 7
                                    else 17
                              else if x <= 119989
                                then 7
                                else if x <= 119993
                                  then 7
                                  else if x <= 119995
                                    then if x <= 119994
                                      then 17
                                      else 7
                                    else 17
                        else if x <= 120057
                          then if x <= 120027
                            then if x <= 120012
                              then if x <= 120004
                                then if x <= 120000
                                  then 7
                                  else if x <= 120002
                                    then 7
                                    else if x <= 120003
                                      then 7
                                      else 17
                                else 7
                              else 7
                            else 7
                          else if x <= 120088
                            then if x <= 120073
                              then if x <= 120065
                                then 7
                                else if x <= 120069
                                  then 7
                                  else if x <= 120071
                                    then if x <= 120070
                                      then 17
                                      else 7
                                    else 7
                              else if x <= 120081
                                then if x <= 120077
                                  then if x <= 120075
                                    then if x <= 120074
                                      then 7
                                      else 17
                                    else if x <= 120076
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 120085
                                  then if x <= 120083
                                    then 7
                                    else if x <= 120084
                                      then 7
                                      else 17
                                  else 7
                            else if x <= 120103
                              then if x <= 120096
                                then if x <= 120092
                                  then 7
                                  else if x <= 120094
                                    then if x <= 120093
                                      then 17
                                      else 7
                                    else 7
                                else 7
                              else 7
                      else if x <= 120240
                        then if x <= 120179
                          then if x <= 120149
                            then if x <= 120134
                              then if x <= 120126
                                then if x <= 120122
                                  then if x <= 120120
                                    then 7
                                    else if x <= 120121
                                      then 7
                                      else 17
                                  else 7
                                else if x <= 120130
                                  then if x <= 120128
                                    then if x <= 120127
                                      then 17
                                      else 7
                                    else 7
                                  else if x <= 120132
                                    then 7
                                    else if x <= 120133
                                      then 17
                                      else 7
                              else if x <= 120142
                                then if x <= 120138
                                  then if x <= 120136
                                    then 17
                                    else if x <= 120137
                                      then 17
                                      else 7
                                  else 7
                                else if x <= 120146
                                  then if x <= 120144
                                    then 7
                                    else if x <= 120145
                                      then 17
                                      else 7
                                  else 7
                            else 7
                          else 7
                        else 7
                  else if x <= 120851
                    then if x <= 120607
                      then if x <= 120485
                        then 7
                        else if x <= 120546
                          then if x <= 120516
                            then if x <= 120501
                              then if x <= 120493
                                then if x <= 120489
                                  then if x <= 120487
                                    then 17
                                    else 7
                                  else 7
                                else 7
                              else if x <= 120509
                                then 7
                                else if x <= 120513
                                  then if x <= 120511
                                    then 7
                                    else if x <= 120512
                                      then 7
                                      else 17
                                  else 7
                            else if x <= 120531
                              then 7
                              else if x <= 120539
                                then if x <= 120535
                                  then 7
                                  else if x <= 120537
                                    then 7
                                    else if x <= 120538
                                      then 7
                                      else 17
                                else 7
                          else if x <= 120577
                            then if x <= 120562
                              then 7
                              else if x <= 120570
                                then 7
                                else if x <= 120574
                                  then if x <= 120572
                                    then if x <= 120571
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                            else if x <= 120592
                              then 7
                              else if x <= 120600
                                then if x <= 120596
                                  then 7
                                  else if x <= 120598
                                    then if x <= 120597
                                      then 17
                                      else 7
                                    else 7
                                else 7
                      else if x <= 120729
                        then if x <= 120668
                          then if x <= 120638
                            then if x <= 120623
                              then 7
                              else if x <= 120631
                                then if x <= 120627
                                  then 7
                                  else if x <= 120629
                                    then if x <= 120628
                                      then 7
                                      else 17
                                    else 7
                                else 7
                            else if x <= 120653
                              then 7
                              else if x <= 120661
                                then if x <= 120657
                                  then if x <= 120655
                                    then if x <= 120654
                                      then 7
                                      else 17
                                    else 7
                                  else 7
                                else 7
                          else if x <= 120699
                            then if x <= 120684
                              then 7
                              else if x <= 120692
                                then if x <= 120688
                                  then if x <= 120686
                                    then 7
                                    else if x <= 120687
                                      then 17
                                      else 7
                                  else 7
                                else 7
                            else if x <= 120714
                              then if x <= 120707
                                then 7
                                else if x <= 120711
                                  then 7
                                  else if x <= 120713
                                    then if x <= 120712
                                      then 7
                                      else 17
                                    else 7
                              else 7
                        else if x <= 120790
                          then if x <= 120760
                            then if x <= 120745
                              then if x <= 120737
                                then 7
                                else if x <= 120741
                                  then 7
                                  else if x <= 120743
                                    then 7
                                    else if x <= 120744
                                      then 7
                                      else 17
                              else 7
                            else if x <= 120775
                              then if x <= 120768
                                then 7
                                else if x <= 120772
                                  then if x <= 120770
                                    then 7
                                    else if x <= 120771
                                      then 17
                                      else 7
                                  else 7
                              else if x <= 120783
                                then if x <= 120779
                                  then 7
                                  else 17
                                else 17
                          else 17
                    else 17
              else if x <= 123297
                then if x <= 122319
                  then 17
                  else if x <= 122808
                    then if x <= 122564
                      then 17
                      else if x <= 122686
                        then if x <= 122625
                          then if x <= 122595
                            then 17
                            else if x <= 122610
                              then 17
                              else if x <= 122618
                                then 17
                                else if x <= 122622
                                  then 17
                                  else if x <= 122624
                                    then if x <= 122623
                                      then 17
                                      else 7
                                    else 7
                          else if x <= 122656
                            then if x <= 122641
                              then if x <= 122633
                                then 7
                                else if x <= 122637
                                  then if x <= 122635
                                    then if x <= 122634
                                      then 17
                                      else 7
                                    else 7
                                  else 7
                              else if x <= 122649
                                then 7
                                else if x <= 122653
                                  then 7
                                  else if x <= 122655
                                    then if x <= 122654
                                      then 7
                                      else 17
                                    else 17
                            else if x <= 122671
                              then if x <= 122664
                                then if x <= 122660
                                  then 17
                                  else 7
                                else if x <= 122668
                                  then if x <= 122666
                                    then 7
                                    else 17
                                  else 17
                              else 17
                        else 17
                    else 17
                else if x <= 124276
                  then 17
                  else if x <= 124765
                    then 17
                    else if x <= 125010
                      then 17
                      else if x <= 125132
                        then 17
                        else if x <= 125193
                          then if x <= 125163
                            then 17
                            else if x <= 125178
                              then 17
                              else if x <= 125186
                                then if x <= 125182
                                  then 17
                                  else if x <= 125184
                                    then if x <= 125183
                                      then 17
                                      else 7
                                    else 7
                                else 7
                          else if x <= 125224
                            then 7
                            else if x <= 125239
                              then 7
                              else if x <= 125247
                                then 7
                                else if x <= 125251
                                  then 7
                                  else 17
    else 17
discriminatorTrans 13 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then 13
                            else if x <= 46
                              then if x <= 38
                                then if x <= 34
                                  then if x <= 32
                                    then 13
                                    else if x <= 33
                                      then 13
                                      else 1
                                  else 13
                                else 13
                              else 13
                          else if x <= 92
                            then if x <= 77
                              then 13
                              else if x <= 85
                                then 13
                                else if x <= 89
                                  then 13
                                  else if x <= 91
                                    then 13
                                    else 14
                            else 13
                        else 13
                      else 13
                    else 13
                  else 13
                else 13
              else 13
            else 13
          else 13
        else 13
      else 13
    else 13
discriminatorTrans 14 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then 17
                            else if x <= 46
                              then if x <= 38
                                then if x <= 34
                                  then if x <= 32
                                    then 17
                                    else if x <= 33
                                      then 17
                                      else 13
                                  else 17
                                else 17
                              else 17
                          else if x <= 92
                            then if x <= 77
                              then 17
                              else if x <= 85
                                then 17
                                else if x <= 89
                                  then 17
                                  else if x <= 91
                                    then 17
                                    else 13
                            else 17
                        else 17
                      else 17
                    else 17
                  else 17
                else 17
              else 17
            else 17
          else 17
        else 17
      else 17
    else 17
discriminatorTrans 15 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then 17
                            else if x <= 46
                              then if x <= 38
                                then 17
                                else if x <= 42
                                  then if x <= 40
                                    then 17
                                    else if x <= 41
                                      then 17
                                      else 18
                                  else 17
                              else if x <= 54
                                then if x <= 50
                                  then if x <= 48
                                    then if x <= 47
                                      then 9
                                      else 17
                                    else 17
                                  else 17
                                else 17
                          else 17
                        else 17
                      else 17
                    else 17
                  else 17
                else 17
              else 17
            else 17
          else 17
        else 17
      else 17
    else 17
discriminatorTrans 16 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then 18
                            else if x <= 46
                              then if x <= 38
                                then 18
                                else if x <= 42
                                  then if x <= 40
                                    then 18
                                    else if x <= 41
                                      then 18
                                      else 16
                                  else 18
                              else if x <= 54
                                then if x <= 50
                                  then if x <= 48
                                    then if x <= 47
                                      then 6
                                      else 18
                                    else 18
                                  else 18
                                else 18
                          else 18
                        else 18
                      else 18
                    else 18
                  else 18
                else 18
              else 18
            else 18
          else 18
        else 18
      else 18
    else 18
discriminatorTrans 18 x =
  if x <= -1
  then -1
  else if x <= 125254
    then if x <= 62627
      then if x <= 31313
        then if x <= 15656
          then if x <= 7828
            then if x <= 3914
              then if x <= 1957
                then if x <= 978
                  then if x <= 489
                    then if x <= 244
                      then if x <= 122
                        then if x <= 61
                          then if x <= 30
                            then 18
                            else if x <= 46
                              then if x <= 38
                                then 18
                                else if x <= 42
                                  then if x <= 40
                                    then 18
                                    else if x <= 41
                                      then 18
                                      else 16
                                  else 18
                              else 18
                          else 18
                        else 18
                      else 18
                    else 18
                  else 18
                else 18
              else 18
            else 18
          else 18
        else 18
      else 18
    else 18
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
