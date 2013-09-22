-- |FPrelude.hs
--
-- Floha Prelude - combinators and handy functions for Floha.
--
-- Copyright (C) 2013 Serguey Zefirov.

{-# LANGUAGE TypeOperators, TemplateHaskell #-}

module Language.Floha.FPrelude where

import Language.Floha.Base

-------------------------------------------------------------------------------
-- Types for packetized streams.

data SOP = NoSOP | SOP
	deriving (Eq, Ord, Show)

data EOP = NoEOP | EOP
	deriving (Eq, Ord, Show)

$(deriveBitRepr [''SOP, ''EOP])

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

-- |Folding actor for packetized streams.
-- It won't work properly if there's data between final EOP and initial SOP.
-- (but it is not valid Avalon stream anyway)
foldA :: BitRepr b => b -> (FE a -> FE b -> FE b) -> Actor ((SOP, EOP, a) :. Nil) (b :. Nil)
foldA b0 foldF = actorN "fold" ("a" :. Nil) $ \(a :. Nil) -> do
	b <- autoN "b"
	initial b b0
	o <- autoN "o"
	rules ((a, b, foldF b a) --> (b,o))
		-- if we encounter EOP, output next value, remember initial b0.
		[ (tuple (__, feEOP, a), b) --> (constant b0, b)
		-- no EOP, no output.
		, (tuple (__, __, a), b) --> (b, __)
		]

-------------------------------------------------------------------------------
-- Actors for packetized streams (with SOP and EOP).

