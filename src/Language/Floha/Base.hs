-- |Base.hs
--
-- Base definitions for Floha.
--
-- Copyright (C) 2013 Serguey Zefirov.

{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, PatternGuards, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Floha.Base
	( (:.)(..)
	, Nil(..)
	, BitRepr(..)
	, FE(..)
	, (-->)
	, LiftFE
	, Tuple(..)
	, Actor(..)
	, actor
	, actorN
	, ActorBody
	, auto
	, autoN
	, initial
	, __
	, rules
	, constant
	, Logic(..)
	, AddSub(..)
	, net
	, deriveBitRepr
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

newtype NatI = NatI { fromNatI :: Integer}
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
	bitReprSize :: a -> NatI
	bitReprSize a = natValue (typeBitSize a)

	-- |Safe value. In most cases @decode 0@ suffice, but
	-- for "one of N" encoding this is not the case.
	-- "One if N" always has one bit set.
	safeValue :: a

	-- |Encode the value as bit vector.
	encode :: a -> BitVectConst

	-- |Decode the value from bit vector.
	decode :: BitVectConst -> a

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
	FELow :: SizedLFE -> FE a

-- |Untyped and sized expressions for generating Verilog or VHDL code.
-- Every expression is accompanied by its size.
data LowFE =
		LFEConst	BitVectConst
	|	LFEVar		VarID
	|	LFEWild
	|	LFEReady	(Maybe VarID)
	|	LFEBin		LFWBinOp	SizedLFE	SizedLFE
	|	LFECat		[SizedLFE]
	deriving (Eq, Ord, Show)

-- |Binary operations for LowFE. Note that there is no distinction between bitwise and logical operations.
-- This is so because they are identical for Verilog and can be made so for VHDL.
data LFWBinOp = Plus | Minus | And | Or | Xor
	deriving (Eq, Ord, Show)

-- |Sized low FE is a pair of LowFE value and it's size.
type SizedLFE = (NatI, LowFE)

-- |Class to describe basic arithmetic operations.
class AddSub a where
	(.+), (.-) :: a -> a -> a

-- |How FE supports them.
instance AddSub a => AddSub (FE a) where
	FELow a@(size,_) .+ FELow b = FELow $ (size, LFEBin Plus a b)
	FELow a@(size,_) .- FELow b = FELow $ (size, LFEBin Minus a b)

-- |Class to describe bitwise operations like .&. and .|. from Data.Bits.Bits.
-- It is also suitable for logical connectives.
class Logic a where
	(.&), (.|) ,(.^) :: a -> a -> a

-- |Support for BitWise for FE's.
instance Logic a => Logic (FE a) where
	FELow a@(size,_) .& FELow b = FELow (size, LFEBin And a b)
	FELow a@(size,_) .| FELow b = FELow (size, LFEBin Or a b)
	FELow a@(size,_) .^ FELow b = FELow (size, LFEBin Xor a b)

-- |Lift HList to the HList of Floha expressions.
type family LiftFE ts
type instance LiftFE Nil = Nil
type instance LiftFE (t :. ts) = FE t :. LiftFE ts

-- |Make a HList of Strings from a HList.
type family StringList ts
type instance StringList Nil = Nil
type instance StringList (n :. ns) = String :. StringList ns

class Tuple tup where
	-- |Transformation at type level - from (FE a, FE b) to FE (a,b)
	type FETuple tup

	-- |The code for transformation.
	tuple :: tup -> FETuple tup


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

auto :: BitRepr a => ActorBodyM (FE a)
auto = inventVar Nothing

autoN :: BitRepr a => String -> ActorBodyM (FE a)
autoN = inventVar . Just

__ :: BitRepr a => FE a
__ = r
	where
		r = FELow (size, LFEWild)
		toA :: BitRepr a => FE a -> a
		toA _ = error "toA in __ is forced!"
		size = fromIntegral $ fromNatI $ bitReprSize $ toA r

-- |Create a constant FE.
constant :: BitRepr a => a -> FE a
constant a = FELow (fromIntegral $ fromNatI $ bitReprSize a, LFEConst $ encode a)

-- |Initial assignment for variable.
initial :: BitRepr a => FE a -> a -> ActorBodyM ()
initial (FELow (_,LFEVar vid)) a = do
	_setInitial vid (bitReprSize a, LFEConst (encode a))

-------------------------------------------------------------------------------
-- Implementation.

data ABState = ABS {
	-- |Generating unique indices.
	  absUnique		:: Int
	-- |Initial values for the variables.
	, absInitials		:: Map.Map VarID SizedLFE
	-- |Ready signals for match header tuple element positions.
	, absMatchReady		:: Map.Map Int SizedLFE
	-- |Register signals as inputs.
	, absInputs		:: Set.Set SizedLFE
	-- |And also register signals as outputs.
	, absOutputs		:: Set.Set SizedLFE
	}

type ActorBodyM a = State ABState a

type ActorBody ins outs = ins -> ActorBodyM outs

startABState :: ABState
startABState = ABS {
	  absUnique		= 0
	, absInitials		= Map.empty
	, absMatchReady		= Map.empty
	, absInputs		= Set.empty
	, absOutputs		= Set.empty
	}

_unique :: ActorBodyM Int
_unique = modify (\abs -> abs { absUnique = absUnique abs + 1 }) >> liftM absUnique get

_inventVarID :: Maybe String -> ActorBodyM VarID
_inventVarID name = liftM (VarID name) _unique

_setInitial :: VarID -> SizedLFE -> ActorBodyM ()
_setInitial v init = modify $ \abs -> abs { absInitials = Map.insert v init $ absInitials abs }

inventVar :: BitRepr a => Maybe String -> ActorBodyM (FE a)
inventVar name = do
	v <- _inventVarID name
	let (r, initial) = mkR v
	_setInitial v initial
	return r
	where
		mkR :: BitRepr a => VarID -> (FE a, SizedLFE)
		mkR varID = (r, initial)
			where
				r = FELow (size, LFEVar varID)
				toA :: BitRepr a => FE a -> a
				toA _ = error "toA was forced!"
				size = fromIntegral $ fromNatI $ bitReprSize $ toA r
				initial = (size, LFEConst $ encode $ safeValue `asTypeOf` toA r)

_setReady :: FE a -> Int -> ActorBodyM ()
_setReady (FELow lfe) n = do
	r <- computeReady lfe
	modify $ \abs -> abs { absMatchReady = Map.insert n r $ absMatchReady abs }
	where
		computeReady (_, LFEVar v) = return (1,LFEReady $ Just v)
		computeReady (_, LFEBin _ a b) =
			liftM2 (\a b -> (1, LFEBin And a b)) (computeReady a) (computeReady b)
		computeReady e = error $ "internal: compute ready does not handle "++show e

class FEList feList where
	inventList :: Maybe (StringList feList) -> ActorBodyM feList

instance FEList Nil where
	inventList _ = return Nil

instance (BitRepr a, FEList feList) => FEList (FE a :. feList) where
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
	_matchHeader (FELow (size,lfe)) = matchLFE lfe
		where
			matchLFE lfe = case lfe of
				LFEConst a -> error "constant in the left side of rule header."
				LFEVar v -> return ()
				LFEWild -> error "wildcard in the left side of rule header."
				LFEBin _ (_,a) (_,b) -> matchLFE a >> matchLFE b
	_matchReady (FELow sizedLFE) = do
		(c, renames) <- mkReady sizedLFE
		return (FELow c, renames)
		where
			mkReady (size, lfe) = case lfe of
				LFEVar a -> _inventVarID Nothing >>= return . (,) (1, LFEReady $ Just a)  . Map.singleton a
				LFEWild -> return (lfe', Map.empty)
					where
						FELow lfe' = constant True
				LFEBin _ a b -> liftM2 combine (mkReady a) (mkReady b)
					where
						combine (r1, d1) (r2, d2) = ((1,LFEBin And r1 r2), Map.unionWithKey (\k -> error $ "non-linear match for "++show k) d1 d2)

instance Change (FE a) where
	_changeHeader (FELow (_, lfe)) = case lfe of
		LFEVar _ -> return ()
		_ -> error "only variables are allowed in right hand of rule header."
	_changeRename map (FELow (size, lfe)) = FELow $ (,) size $ renameLFE lfe
		where
			renameLFE lfe = case lfe of
				LFEVar varid 
					| Just v' <- Map.lookup varid map -> LFEVar v'
					| otherwise -> lfe
				LFEBin op (size1, a) (size2, b) -> LFEBin op (size1, renameLFE a) (size2, renameLFE b)
				_ -> lfe
	_change e = return $ constant False

-------------------------------------------------------------------------------
-- Instances of various classes for various types.

instance BitRepr Bool where
	type BitSize Bool = S Z
	safeValue = False
	encode = BitVectConst . fromIntegral . fromEnum
	decode = toEnum . fromIntegral . fromBitVectConst

instance Logic Bool where
	(.&) = (&&)
	(.|) = (||)
	a .^ b = (a .& not b) .| (not a .& b)

-- |Derive instances of BitRepr for algebraic types.
deriveBitRepr :: [TH.Name] -> TH.Q [TH.Dec]
deriveBitRepr names = return []

-------------------------------------------------------------------------------
-- Trivial derivable instance for header tuples, BitRepr, etc.

$(liftM concat $ forM [2..8] $ \n -> let
		names = map (TH.mkName . ("a"++) . show) [1..n]
		types = map TH.VarT names
		tupleP = TH.TupP (map TH.VarP names)
		tupleType = tupleT types
		fes = map (TH.ConT (''FE) `TH.AppT`) types
		matchN = ''Match
		changeN = ''Change
		matchC t = TH.ConT (''Match) `TH.AppT` t
		changeC t = TH.ConT (''Change) `TH.AppT` t
		tupleC t = TH.ConT (''Tuple) `TH.AppT` t
		bitReprC t = TH.ConT (''BitRepr) `TH.AppT` t
		matchPred t = TH.ClassP matchN [t]
		changePred t = TH.ClassP changeN [t]
		bitReprPred t = TH.ClassP ''BitRepr [t]
		tupleT ts = foldl TH.AppT (TH.TupleT (length ts)) ts
		tupleFEs = tupleT fes
		_matchHeaderCode = TH.FunD '_matchHeader
			[TH.Clause [tupleP] (TH.NormalB $ TH.DoE stmts) []]
			where
				stmts = zipWith (\name ix -> TH.NoBindS $ TH.VarE '_setReady `TH.AppE` TH.VarE name `TH.AppE` TH.LitE (TH.IntegerL ix))
					names [0..]
		match = TH.InstanceD (map matchPred fes) (matchC tupleFEs) [_matchHeaderCode]
		change = TH.InstanceD (map changePred fes) (changeC tupleFEs) []
		tuple = TH.InstanceD [] (tupleC tupleFEs) []
		bitRepr = TH.InstanceD (TH.ClassP ''Nat [TH.ConT ''BitSize `TH.AppT` tupleType]:map bitReprPred types) (bitReprC tupleType) []
	in return [match, change, tuple, bitRepr]
 )

