{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Naturales where

import Prelude (Show)

data Bool where { True :: Bool; False :: Bool } deriving Show
data N where { O :: N; S :: N -> N } deriving Show

not :: Bool -> Bool
not = \b -> case b of { True -> False; False -> True }

(&&) :: Bool -> Bool -> Bool
(&&) = \b1 -> \b2 -> case b1 of {
                    True -> b2;
                    False -> False
}

cero = O
uno = S cero
dos = S uno
tres = S dos
cuatro = S tres
cinco = S cuatro
seis = S cinco

predecesor :: N -> N
predecesor = \n -> case n of { O -> O; S x -> x }

par :: N -> Bool
par = \n -> case n of { O -> True; S x -> not (par x) }

doble :: N -> N
doble = \n -> case n of { O -> O; S x -> S (S (doble x)) }

existe :: N -> (N -> Bool) -> Bool
existe = \n -> \p -> case n of { O -> p O; S x -> case p (S x) of { False -> existe x p; True -> True } }
--redefinir usando conectivos booleanos, sale con or

todos :: N -> (N -> Bool) -> Bool
todos = \n -> \p -> case n of { O -> p O; S x -> case p (S x) of { False -> False; True -> todos x p } }
-- redefinir usando conectivos booleanos, sale con and

contar :: N -> (N -> Bool) -> N
contar = \n -> \p -> case n of { O -> case p O of { True -> S O; False -> O }; S x -> case p (S x) of { True -> S (contar x p); False -> contar x p } }

(+) :: N -> N -> N
(+) = \n -> \m -> case n of { O -> m; S x -> S (x + m)}

(*) :: N -> N -> N
(*) = \n -> \m -> case n of { O -> O; S x -> m + (x * m) }

sumi :: N -> N
sumi = \n -> case n of { O -> O; S x -> S x + sumi x }

sumdobles :: N -> N
sumdobles = \n -> case n of { O -> O; S x -> doble (S x) + sumdobles x }

sumfi :: N -> (N -> N) -> N
sumfi = \n -> \f -> case n of { O -> O; S x -> f (S x) + sumfi x f }

sumparesi :: N -> N
sumparesi = \n -> case n of { O -> O; S x -> case par (S x) of { True -> S x + sumparesi x; False -> sumparesi x } }

sumpi :: N -> (N -> Bool) -> N
sumpi = \n -> \p -> case n of { O -> O; S x -> case p (S x) of { True -> S x + sumpi x p; False -> sumpi x p } }