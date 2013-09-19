-- |Base.hs
--
-- Base definitions for Floha.
--
-- Copyright (C) 2013 Serguey Zefirov.

{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, PatternGuards #-}

module Language.Floha.Base
	( (:.)(..)
	, Nil(..)
	, FE(..)
	, (-->)
	, LiftFE
	, Actor(..)
	, actor
	, actorN
	, ActorBody
	, auto
	, autoN
	, rules
	, net
	) where

import Control.Monad
import Control.Monad.State

import Data.Bits

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Language.Haskell.TH as TH

-------------------------------------------------------------------------------
-- HList.

infixr 5 :.
data a :. b = a :. b
data Nil = Nil

-------------------------------------------------------------------------------
-- Constraints on types for Floha.
-- Basically, the bit size of value of type should be computable at the compilation time.

data Z = Z
data S n = S n

type family Plus a b
type instance Plus Z b = b
type instance Plus (S a) b = S (Plus a b)

newtype NatI = NatI Integer
	deriving (Eq, Ord, Show, Num)

class Nat a where
	-- |Get the value of that type.
	realizeNat :: a

	-- |Lower value from types.
	natValue :: a -> NatI

instance Nat Z where
	realizeNat = Z
	natValue ~Z = 0

instance Nat n => Nat (S n) where
	realizeNat = S realizeNat
	natValue ~(S n) = 1 + natValue n

newtype BitVectConst = BitVectConst { fromBitVectConst :: Integer }
	deriving (Eq, Ord, Show, Num, Bits)

class Nat (BitSize a) => BitRepr a where
	-- |Type representation for the size.
	type BitSize a

	typeBitSize :: a -> BitSize a
	typeBitSize _ = realizeNat

	-- |Get the bit size of the type.
	bitSize :: a -> NatI
	bitSize a = natValue (typeBitSize a)

	-- |Safe value. In most cases @decode 0@ suffice, but
	-- for "one of N" encoding this is not the case.
	-- "One if N" always has one bit set.
	safeValue :: a

	-- |Encode the value as bit vector.
	encode :: a -> BitVectConst

	-- |Decode the value from bit vector.
	decode :: BitVectConst -> a

instance BitRepr Bool where
	type BitSize Bool = S Z
	safeValue = False
	encode = BitVectConst . fromIntegral . fromEnum
	decode = toEnum . fromIntegral . fromBitVectConst

-------------------------------------------------------------------------------
-- HList.

-- |How we identify vars.
data VarID = VarID (Maybe String) Int
	deriving (Eq, Ord, Show)

-- |Compile-time constant size bit vectors are a necessity.
data BitVect size = BitVect
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
	FEBin :: BinOp left right result -> FE left -> FE right -> FE result

-- |Binary operations.
data BinOp left right result where
	Plus :: AddSub a => BinOp a a a
	Minus :: AddSub a => BinOp a a a
	Concat :: BinOp (BitVect lsize) (BitVect rsize) (BitVect (Plus lsize rsize))
	And :: BinOp Bool Bool Bool
	Or :: BinOp Bool Bool Bool
	BWAnd :: BitWise a => BinOp a a a
	BWOr :: BitWise a => BinOp a a a
	BWXor :: BitWise a => BinOp a a a

-- |Untyped and sized expressions for generating Verilog or VHDL code.
-- Every expression is accompanied by its size.
data LowFE =
		LFEConst	Integer
	|	LFEVar		VarID
	|	LFEBin		LFWBinOp	SizedLFE	SizedLFE
	deriving (Eq, Ord, Show)

-- |Binary operations for LowFE. Note that there is no distinction between bitwise and logical operations.
-- This is so because they are identical for Verilog and can be made so for VHDL.
data LFWBinOp = LBPlus | LBMinus | LBAnd | LBOr | LBXor
	deriving (Eq, Ord, Show)

-- |Sized low FE is a pair of LowFE value and it's size.
type SizedLFE = (LowFE, Int)

-- |Class to describe basic arithmetic operations.
class AddSub a where
	(.+), (.-) :: a -> a -> a

-- |How FE supports them.
instance AddSub a => AddSub (FE a) where
	(.+) = FEBin Plus
	(.-) = FEBin Minus

-- |Class to describe bitwise operations like .&. and .|. from Data.Bits.Bits.
class BitWise a where
	(.&), (.|) ,(.^) :: a -> a -> a

-- |Support for BitWise for FE's.
instance BitWise a => BitWise (FE a) where
	(.&) = FEBin BWAnd
	(.|) = FEBin BWOr
	(.^) = FEBin BWXor

-- |Lift HList to the HList of Floha expressions.
type family LiftFE ts
type instance LiftFE Nil = Nil
type instance LiftFE (t :. ts) = FE t :. LiftFE ts

-- |Make a HList of Strings from a HList.
type family StringList ts
type instance StringList Nil = Nil
type instance StringList (n :. ns) = String :. StringList ns

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
	ins <- inventList Nothing
	outs <- body ins
	return $ Actor name ins outs

actorN :: (FEList (LiftFE ins), FEList (LiftFE outs)) => String -> StringList (LiftFE ins) -> ActorBody (LiftFE ins) (LiftFE outs) -> Actor ins outs
actorN name names body = flip evalState startABState $ do
	ins <- inventList $ Just names
	outs <- body ins
	return $ Actor name ins outs

net :: String -> Actor ins outs
net name = error "net is not yet ready."

rules :: (Match matchE, Change changeE) => (matchE, changeE) -> [(matchE, changeE)] -> ActorBodyM ()
rules (headerMatch, headerResults) matchChanges = do
	_matchHeader headerMatch
	_changeHeader headerResults
	error "rules not yet done!"

-- | @-->@ is just an operator version of tuple constructor.
infix 9 -->
(-->) :: a -> b -> (a,b)
a --> b = (a,b)

auto :: ActorBodyM (FE a)
auto = inventVar Nothing

autoN :: String -> ActorBodyM (FE a)
autoN = inventVar . Just

__ :: FE a
__ = FEWild

constant :: BitRepr a => a -> FE a
constant = FEConst

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

inventVar :: Maybe String -> ActorBodyM (FE a)
inventVar name = liftM (FEVar . VarID name) _unique

class FEList feList where
	inventList :: Maybe (StringList feList) -> ActorBodyM feList

instance FEList Nil where
	inventList _ = return Nil

instance FEList feList => FEList (FE a :. feList) where
	inventList names = do
		fe <- inventVar name
		feList <- inventList names'
		return $ fe :. feList
		where
			(name, names') = case names of
				Just (n :. ns) -> (Just n, Just ns)
				Nothing -> (Nothing, Nothing)


class Match match where
	-- |Check that header is OK.
	_matchHeader :: match -> ActorBodyM ()
	-- |Compute the readyness signal from match expression and readyness of the corresponding header expression.
	-- Bind matched variables. Rename variables if needed.
	_matchReady :: match -> ActorBodyM (FE Bool, Map.Map VarID VarID)

class Change change where
	-- |Check header of the right hand of @rules@. Should be a variable, nothing else, because we assign there.
	_changeHeader :: change -> ActorBodyM ()
	-- |Rename bound variables.
	_changeRename :: Map.Map VarID VarID -> change -> change
	-- |Perform the computation. Return the "ready" flag.
	_change :: change -> ActorBodyM (FE Bool)

instance Match (FE a) where
	_matchHeader fe = case fe of
		FEConst a -> error "constant in the left side of rule header."
		FEVar v -> return ()
		FEWild -> error "wildcard in the left side of rule header."
		FEBin _ _ _ -> return ()

instance Change (FE a) where
	_changeHeader fe = case fe of
		FEVar _ -> return ()
		_ -> error "only variables are allowed in right hand of rule header."
	_changeRename map e@(FEVar v)
		| Just v' <- Map.lookup v map = FEVar v'
		| otherwise = e
	_changeRename map e = case e of
		FEBin op a b -> FEBin op (_changeRename map a) (_changeRename map b)
		_ -> e
	_change e = return $ constant False

$(liftM concat $ forM [2..8] $ \n -> let
		names = map (TH.mkName . ("a"++) . show) [1..n]
		types = map TH.VarT names
		fes = map (TH.ConT (TH.mkName "FE") `TH.AppT`) types
		matchN = TH.mkName "Match"
		changeN = TH.mkName "Change"
		matchC t = TH.ConT (TH.mkName "Match") `TH.AppT` t
		changeC t = TH.ConT (TH.mkName "Change") `TH.AppT` t
		matchPred t = TH.ClassP matchN [t]
		changePred t = TH.ClassP changeN [t]
		tupleT ts = foldl TH.AppT (TH.TupleT (length ts)) ts
		match = TH.InstanceD (map matchPred fes) (matchC $ tupleT fes) []
		change = TH.InstanceD (map changePred fes) (changeC $ tupleT fes) []
	in return [match, change]
 )
