{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module IntroBool where

import Prelude (Show)

data Bool where { True :: Bool; False :: Bool } deriving Show

idb :: Bool -> Bool
idb = \b -> b

kT :: Bool -> Bool
kT = \b -> True

kF :: Bool -> Bool
kF = \b -> False

not :: Bool -> Bool
not = \b -> case b of { True -> False; False -> True }

and :: Bool -> Bool -> Bool
and = \b1 -> \b2 -> case b1 of {
                    True -> case b2 of { True -> True; False -> False };
                    False -> False
}

(&&) :: Bool -> Bool -> Bool
(&&) = \b1 -> \b2 -> case b1 of {
                    True -> b2;
                    False -> False
}

-- :l myFiles/bool.hs
-- idb True
-- kT False
-- not True
-- and True False
-- and True True
-- True && False
-- True && True
-- True && not False