-- |Base.hs
--
-- Base definitions for Floha.
--
-- Copyright (C) 2013 Serguey Zefirov.

{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Floha.Base
	( (:.)(..)
	, Nil(..)
	, FE(..)
	, (-->)
	, LiftFE
	, Actor(..)
	, actor
	, ActorBody
	, auto
	, rules
	, net
	) where

import Control.Monad
import Control.Monad.State

import Data.Bits

import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- HList.

infixr 5 :.
data a :. b = a :. b
data Nil = Nil

-------------------------------------------------------------------------------
-- Constraints on types for Floha.
-- Basically, the bit size of value of type should be computable at the compilation time.

newtype NatI = NatI Integer
	deriving (Eq, Ord, Show, Num)

class Nat a where
	-- |Get the value of that type.
	realizeNat :: a

	-- |Lower value from types.
	natValue :: a -> NatI

newtype BitVectConst = BitVectConst Integer
	deriving (Eq, Ord, Show, Num, Bits)

class Nat (BitSize a) => BitRepr a where
	-- |Type representation for the size.
	type BitSize a

	typeBitSize :: a -> BitSize a
	typeBitSize _ = realizeNat

	-- |Get the bit size of the type.
	bitSize :: a -> NatI
	bitSize a = natValue (typeBitSize a)

	-- |Encode the value as bit vector.
	encode :: a -> BitVectConst

	-- |Decode the value from bit vector.
	decode :: BitVectConst -> a

-------------------------------------------------------------------------------
-- HList.

data VarID = VarID (Maybe String) Int
	deriving (Eq, Ord, Show)

-- |Floha expression structure.
data FE a where
	-- |Constant expression.
	FEConst :: BitRepr a => a -> FE a
	-- |Variable.
	FEVar :: VarID -> FE a
	-- |Wildcard. Wildcards can be seen in both sides of rules.
	-- In left side it means that we won't check readyness of appropriate
	-- header expression.
	-- In the right side it means that we won't change appropriate header
	-- variable or output.
	FEWild :: FE a

-- |Lift HList to the HList of Floha expressions.
type family LiftFE ts
type instance LiftFE Nil = Nil
type instance LiftFE (t :. ts) = FE t :. LiftFE ts

-- |Floha actor.
data Actor ins outs where
	-- |Actor is either real actor - a state machine.
	Actor :: String -> LiftFE ins -> LiftFE outs -> Actor ins outs
	-- |Or actor is a network of connections between actors.
	Network :: String -> Actor ins outs

-------------------------------------------------------------------------------
-- Main combinators.

actor :: (FEList (LiftFE ins), FEList (LiftFE outs)) => String -> ActorBody (LiftFE ins) (LiftFE outs) -> Actor ins outs
actor name body = flip evalState startABState $ do
	ins <- inventList
	outs <- body ins
	return $ Actor name ins outs

net :: String -> Actor ins outs
net name = error "net is not yet ready."

rules :: (Match matchE, Change changeE) => (matchE, changeE) -> [(matchE, changeE)] -> ActorBodyM ()
rules direction matchChanges = error "rules not yet done!"

-- | @-->@ is just an operator version of tuple constructor.
infix 9 -->
(-->) :: a -> b -> (a,b)
a --> b = (a,b)

auto :: ActorBodyM (FE a)
auto = inventVar

__ :: FE a
__ = FEWild

-------------------------------------------------------------------------------
-- Implementation.

data ABState = ABS {
	  absUnique		:: Int
	}

type ActorBodyM a = State ABState a

type ActorBody ins outs = ins -> ActorBodyM outs

startABState :: ABState
startABState = ABS {
	  absUnique		= 0
	}

_unique :: ActorBodyM Int
_unique = modify (\abs -> abs { absUnique = absUnique abs + 1 }) >> liftM absUnique get

inventVar :: ActorBodyM (FE a)
inventVar = liftM (FEVar . VarID Nothing) _unique

class FEList feList where
	inventList :: ActorBodyM feList

instance FEList Nil where
	inventList = return Nil

instance FEList feList => FEList (FE a :. feList) where
	inventList = do
		fe <- inventVar
		feList <- inventList
		return $ fe :. feList


class Match match where
	-- |Check that header is OK.
	_matchHeader :: match -> ActorBodyM ()

class Change change where
	_change :: change -> ActorBodyM ()

instance Match (FE a) where
	_matchHeader fe = case fe of
		FEConst a -> error "constant in the left side of rule header."
		FEVar v -> return ()
		FEWild -> error "wildcard in the left side of rule header."
