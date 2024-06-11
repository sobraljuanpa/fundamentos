{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module FS where 

import Prelude
import Data.List

-- Nombre: Arranco a las 1630


type Nombre = String 

data Ext where { Txt::Ext ; Mp3::Ext ; Jar::Ext ; Doc::Ext ; Hs::Ext }
  deriving (Eq, Show)


data FS where {  A :: (Nombre,Ext) -> FS;
                 C :: Nombre -> [FS] -> FS }
  deriving (Eq, Show)

instance Ord FS where
  (<=) = \f1 -> \f2 -> nombre f1 <= nombre f2
	
----
-- 1
cjazz :: FS 
cjazz = C "jazz" [A ("mumbles", Mp3)]

crock :: FS 
crock = C "rock" [ A ("clones", Mp3), A ("bajan", Mp3), A ("clara", Mp3)]

cmusica :: FS
cmusica = C "musica" [cjazz, crock, A ("clara", Mp3)]


-- Completar el resto de los componentes del FS

cort :: FS
cort = C "ort" [cobls, A ("notas", Txt)]

cobls :: FS
cobls = C "obls" [A ("p2", Txt) , A ("p2", Jar), A ("fc", Hs)]

csys :: FS
csys = C "sys" [A ("sys", Txt), C "sys" []]

craiz :: FS
craiz = C "raiz" [cmusica, A ("notas", Txt), cort, csys]

----
-- 2
nombre :: FS -> Nombre
nombre = \f -> case f of
                A (n,e) -> case e of 
                            Txt -> n ++ ".txt"
                            Mp3 -> n ++ ".mp3"
                            Jar -> n ++ ".jar"
                            Doc -> n ++ ".doc"
                            Hs -> n ++ ".hs"
                C n _ -> n
----
-- 3
contenido :: FS -> [Nombre]
contenido = \f -> case f of 
                    A _ -> error "No se pueden listar los contenidos de un archivo"
                    C n l -> case l of 
                              [] -> []
                              x:xs -> nombre x : map nombre xs
----
-- 4
sumL :: [Int] -> Int
sumL = \l -> case l of
                [] -> 0
                x:xs -> x + sumL xs

cantA :: FS -> Int 
cantA = \f -> case f of
                A _ -> 1
                C n l -> sumL (map cantA l)
----
-- 5

orL :: [Bool] -> Bool
orL = \l -> case l of
              [] -> False
              x:xs -> x || orL xs

pertenece :: Nombre -> FS -> Bool 
pertenece = \n -> \f -> case f of
                        A (nom, ext) -> n == nom
                        C nom l -> case n == nom of 
                                        False -> orL (map (pertenece n) l)
                                        True -> True
----
-- 6
andL :: [Bool] -> Bool
andL = \l -> case l of
              [] -> True
              x:xs -> x && andL xs

contenidoo :: String -> [String] -> Bool
contenidoo = \s -> \l -> case l of
                          [] -> False
                          x:xs -> case x == s of
                                      False -> contenidoo s xs
                                      True -> True

repetidos :: [String] -> Bool
repetidos = \l -> case l of 
                    [] -> False
                    x:xs -> case contenidoo x xs of
                                False -> repetidos xs
                                True -> True

valido :: FS -> Bool
valido = \f -> case f of
                A _ -> True
                C _ l -> (not (repetidos (map nombre l))) && andL (map valido l)

----
-- 7
concatL :: [[Nombre]] -> [Nombre]
concatL = \l -> case l of
                  [] -> []
                  x:xs -> x ++ concatL xs
archivosExt :: Ext -> FS -> [Nombre]
archivosExt = \e -> \f -> case f of
                            A (nom, ext) -> case e == ext of
                                              False -> []
                                              True -> [nom]
                            C _ l -> concatL (map (archivosExt e) l)
----
-- 8
cambiarNom :: Nombre -> Nombre -> FS -> FS 
cambiarNom = \n1 -> \n2 -> \f -> case f of
                                  A (nom, ext) -> case nom == n1 of
                                                    False -> A (nom, ext)
                                                    True -> A (n2, ext)
                                  C nom l -> case nom == n1 of
                                                    False -> C nom (map (cambiarNom n1 n2) l)
                                                    True -> C n2 (map (cambiarNom n1 n2) l)		
----
-- 9
nivelesC :: FS -> Int
nivelesC = \f -> case f of
                A _ -> 0
                C _ l -> case l of 
                          [] -> 1
                          x:xs -> 1 + maximum (map nivelesC l)
----
-- 10
borrarPosta :: Nombre -> [FS] -> [FS]
borrarPosta = \n -> \l -> case l of
                      [] -> []
                      x:xs -> case x of
                            A (nom, ext) -> case nombre x == n of
                                            False -> x:borrarPosta n xs
                                            True -> borrarPosta n xs
                            C nom l -> case nom == n of
                                            False -> C nom (borrarPosta n l): borrarPosta n xs
                                            True -> borrarPosta n xs

borrar :: Nombre -> FS -> FS 
borrar = \n -> \f -> case f of
                    A (nom, ext) -> A (nom, ext)
                    C nom l -> C nom (borrarPosta n l)
-----
--11
ordenar :: FS -> FS
ordenar = \f -> case f of
                A _ -> f
                C nom l -> C nom (map ordenar (sort l))
