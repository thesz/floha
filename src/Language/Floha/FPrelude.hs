-- |FPrelude.hs
--
-- Prelude - combinators and handy functions for Floha.
--
-- Copyright (C) 2013 Serguey Zefirov.

{-# LANGUAGE TypeOperators #-}

module Language.Floha.FPrelude where

import Language.Floha.Base

-- |Mapping actor.
mapA :: (FE a -> FE b) -> Actor (a :. Nil) (b :. Nil)
mapA f = actor "map" $ \(i :. Nil) -> do
	o <- auto
	rules (i --> o)
		[ i --> f i]
	return (o :. Nil)

-- |Zipping actor.
-- Our streams are infinite, basicaally. So zipping actor will wait for both
-- values to arrive.
zipWithA :: (FE a -> FE b -> FE c) -> Actor (a :. b :. Nil) (c :. Nil)
zipWithA f = actor "zipWith" $ \(a :. b :. Nil) -> do
	o <- auto
	rules ((a, b) --> o)
		[ (a,b) --> f a b]
	return (o :. Nil)
