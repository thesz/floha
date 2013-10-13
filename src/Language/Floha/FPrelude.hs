-- |FPrelude.hs
--
-- Floha Prelude - combinators and handy functions for Floha.
--
-- Copyright (C) 2013 Serguey Zefirov.

{-# LANGUAGE TypeOperators, TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Language.Floha.FPrelude where

import Language.Floha.Base

-------------------------------------------------------------------------------
-- Types for packetized streams.

data SOP = NoSOP | SOP
	deriving (Eq, Ord, Show)

data EOP = NoEOP | EOP
	deriving (Eq, Ord, Show)

$(deriveBitRepr [''SOP, ''EOP])

type AS a = (SOP, EOP, a)

-------------------------------------------------------------------------------
-- Basic actors.

-- |Identity actor. While simple, it can be useful in some situations.
idA :: BitRepr a => Actor (a :. Nil) (a :. Nil)
idA = actorN "id" ("a" :. Nil) $ \(i :. Nil) -> do
	o <- autoN "o"
	rules (i --> o)
		[ i --> i]
	return (o :. Nil)

-- |Nil actor - accepts anything, produces nothing.
nilA :: BitRepr a => Actor (a :. Nil) Nil
nilA = actorN "nil" ("i" :. Nil) $ \(i :. Nil) -> do
	-- this variable will be optimized away.
	x <- autoN "x"
	rules (i --> (x :: FE ()))
		[i --> x]
	return Nil

-- |Duplication of stream.
dupA :: BitRepr a => Actor (a :. Nil) (a :. a :. Nil) 
dupA = actorN "dup" ("i" :. Nil) $ \(i :. Nil) -> do
	left <- autoN "left"
	right <- autoN "right"
	rules (i --> (left, right))
		[(i --> (i,i))]
	return (left :. right :. Nil)

-- |Mapping actor.
mapA :: (BitRepr a, BitRepr b) => (FE a -> FE b) -> Actor (a :. Nil) (b :. Nil)
mapA f = actorN "map" ("i" :. Nil) $ \(i :. Nil) -> do
	o <- autoN "o"
	rules (i --> o)
		[ i --> f i]
	return (o :. Nil)

-- |Zipping actor.
-- Our streams are infinite, basically. So zipping actor will wait for both
-- values to arrive.
zipWithA :: (BitRepr a, BitRepr b, BitRepr c) => (FE a -> FE b -> FE c) -> Actor (a :. b :. Nil) (c :. Nil)
zipWithA f = actorN "zipWith" ("a" :. "b" :. Nil) $ \(a :. b :. Nil) -> do
	o <- autoN "o"
	rules ((a, b) --> o)
		[ (a,b) --> f a b]
	return (o :. Nil)

-------------------------------------------------------------------------------
-- Actors for packetized streams (with SOP and EOP).

-- |Folding actor for packetized streams.
-- It won't work properly if there's data between final EOP and initial SOP.
-- (but it is not valid Avalon stream anyway)
foldA :: (BitRepr b, BitRepr (AS a), BitRepr a) => b -> (FE a -> FE b -> FE b) -> Actor (AS a :. Nil) (b :. Nil)
foldA b0 foldF = actorN "fold" ("i" :. Nil) $ \(i :. Nil) -> do
	b <- autoN "b"
	a <- autoN "a"
	initial b b0
	o <- autoN "o"
	rules ((i, b) --> (b,o))
		-- if we encounter EOP, output next value, remember initial b0.
		[ (as (__, feEOP, a), b) --> (constant b0, foldF a b)
		-- no EOP, no output.
		, (as (__, __,    a), b) --> (foldF a b,   __)
		]
	return (o :. Nil)
	where
		as :: (BitRepr c, FE (SOP,EOP,c) ~ FETuple (FE SOP, FE EOP, FE c)) => (FE SOP, FE EOP, FE c) -> FE (SOP,EOP,c)
		as (a,b,c) = tuple (a,b,c)

-- |Make an arbitrage between two Avalon streams. Send one completely to the output.
-- Arbitrage is made for fair transmission - if left stream was transmitted, then
-- right stream gets prority, and vice versa.
arbiterA :: (BitRepr a) => Actor (AS a :. AS a :. Nil) (AS a :. Nil)
arbiterA = actorN "arbiter" ("left" :. "right" :. Nil) $ \(left :. right :. Nil) -> do
	o <- autoN "out"
	lastLeft <- autoN "lastLeft"
	initial lastLeft False
	transmit <- autoN "transmit"
	a <- autoN "a"
	rules ((transmit, lastLeft, left, right) --> (transmit, lastLeft, o))
		[ (feFalse, feFalse,  tuple (feSOP, feEOP,   a), __) -->   (feFalse, feTrue, tuple (feSOP, feEOP,   a))
		, (feFalse, feFalse,  tuple (feSOP, feNoEOP, a), __) -->   (feTrue,  feTrue, tuple (feSOP, feNoEOP, a))
		, (feTrue,  lastLeft, tuple (feNoSOP, feNoEOP, a), __) --> (feTrue,  lastLeft, tuple (feNoSOP, feNoEOP, a))
		]
	return (o :. Nil)

