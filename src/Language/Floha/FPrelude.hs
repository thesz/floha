-- |FPrelude.hs
--
-- Floha Prelude - combinators and handy functions for Floha.
--
-- Copyright (C) 2013 Serguey Zefirov.

{-# LANGUAGE TypeOperators #-}

module Language.Floha.FPrelude where

import Language.Floha.Base

-------------------------------------------------------------------------------
-- Basic actors.

-- |Mapping actor.
mapA :: (FE a -> FE b) -> Actor (a :. Nil) (b :. Nil)
mapA f = actorN "map" ("i" :. Nil) $ \(i :. Nil) -> do
	o <- autoN "o"
	rules (i --> o)
		[ i --> f i]
	return (o :. Nil)

-- |Zipping actor.
-- Our streams are infinite, basicaally. So zipping actor will wait for both
-- values to arrive.
zipWithA :: (FE a -> FE b -> FE c) -> Actor (a :. b :. Nil) (c :. Nil)
zipWithA f = actorN "zipWith" ("a" :. "b" :. Nil) $ \(a :. b :. Nil) -> do
	o <- autoN "o"
	rules ((a, b) --> o)
		[ (a,b) --> f a b]
	return (o :. Nil)

-------------------------------------------------------------------------------
-- Actors for packetized streams (with SOP and EOP).

