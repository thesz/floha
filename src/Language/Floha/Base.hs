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

import Debug.Trace

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

type family Max a b
type instance Max Z (S b) = S b
type instance Max (S a) Z = S a
type instance Max (S a) (S b) = S (Max a b)

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
	|	LFEChangeSize	ChangeFiller	(SizedLFE)
	deriving (Eq, Ord, Show)

-- |Binary operations for LowFE. Note that there is no distinction between bitwise and logical operations.
-- This is so because they are identical for Verilog and can be made so for VHDL.
data LFWBinOp = Plus | Minus | And | Or | Xor | Equal | Compare CompareOp
	deriving (Eq, Ord, Show)

-- |Whether comparison is signed?
data Signed = Unsigned | Signed
	deriving (Eq, Ord, Show)

-- |The comparison operator.
data CompareOperator = LT | LE | GT | GE
	deriving (Eq, Ord, Show)

-- |Compare operations.
data CompareOp = CompareOp CompareOperator Signed
	deriving (Eq, Ord, Show)

-- |What we are doing when changing size of expression.
-- This is relevant only for extending the size.
data ChangeFiller = FillZero | FillSign
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

internalUnsafeCast :: (BitRepr a, BitRepr b) => FE a -> FE b
internalUnsafeCast (FELow e) = r
	where
		r = FELow (feBitSize r, LFEChangeSize FillZero e)

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

-- |Handy function to get the size of FE.
feBitSize :: BitRepr a => FE a -> NatI
feBitSize e = bitReprSize $ toA e
	where
		toA :: BitRepr a => FE a -> a
		toA _ = error "toA in feBitSize called!"

-- |Handy function to get the safe value for FE.
feSafeValue :: BitRepr a => FE a -> a
feSafeValue e = safeValue

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
				size = feBitSize r
				initial = (size, LFEConst $ encode $ feSafeValue r)

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

_concatenateSizedBitVectors :: [(NatI, BitVectConst)] -> BitVectConst
_concatenateSizedBitVectors vects = snd $ foldl1 (\(_,acc) (size, x) -> (0,shiftL acc (fromIntegral $ fromNatI size) .|. x)) vects

type family AlgebraicTypeBusSize a

-- |Derive instances of BitRepr for algebraic types.
-- Also derive functions to express construction of algebraic types values as FE's.
deriveBitRepr :: [TH.Name] -> TH.Q [TH.Dec]
deriveBitRepr names = do
	defs <- liftM concat $ forM names $ \name -> do
		info <- TH.reify name
		case info of
			TH.TyConI (TH.DataD [] name vars conses derive) -> do
				fes <- liftM concat $ forM (zip [0..] conses) $ \(i, cons) -> def i (length conses) (maximum $ map (length . conTypes) conses) name vars cons
				return (bitRepr name vars conses ++ fes)
			_ -> error $ show name ++ " is not an algebraic type with empty context."
	TH.runIO $ mapM (print . TH.ppr) defs
	return defs
	where
		completeTy name vars =
			foldl TH.AppT (TH.ConT name) $ map (TH.VarT . fromVarBindr) vars
		fromVarBindr (TH.PlainTV v) = v
		fromVarBindr (TH.KindedTV _ _) = error "kinded type arguments aren't supported."
		conTypes cons = case cons of
			TH.NormalC _ stys -> map snd stys
		bitReprC v = TH.ClassP ''BitRepr [TH.VarT v]
		bitRepr name vars conses =
			[ TH.InstanceD
				(TH.ClassP ''Nat [TH.ConT ''BitSize `TH.AppT` ty] : map bitReprC (map fromVarBindr vars))
				(TH.ConT ''BitRepr `TH.AppT` ty)
				[ TH.TySynInstD ''BitSize [ty] sizeT, safeValueV, encodeF, decodeF]
			, TH.TySynInstD ''AlgebraicTypeBusSize [ty] (plusT selSize (foldl1 maxT conSizes))]
			where
				ty = completeTy name vars
				nToTy 0 = TH.ConT ''Z
				nToTy n = TH.ConT ''S `TH.AppT` nToTy (n-1)
				conSizes = map conSize conses
				conSize cons = case conTypes cons of
					[] -> TH.ConT ''Z
					ts -> foldl1 plusT $ map (TH.ConT ''BitSize `TH.AppT`) ts
				selSize
					| length conses == 1 = nToTy 0
					| length conses == 2 = nToTy 1
					| length conses > 2 = nToTy (length conses)
				plusT a b = TH.ConT ''Plus `TH.AppT` a `TH.AppT` b
				maxT a b = TH.ConT ''Max `TH.AppT` a `TH.AppT` b
				bitSizeT a = TH.ConT ''BitSize `TH.AppT` a
				consSize (TH.NormalC _ []) = []
				consSize (TH.NormalC _ (a:as)) = [foldl plusT (bitSizeT $ snd a) (map (bitSizeT . snd) as)]
				sizeT = case concatMap consSize conses of
					[] -> selSize
					s:ss -> plusT selSize (foldl maxT s ss)
				safeValueV = TH.FunD 'safeValue [TH.Clause [] (TH.NormalB sv) []]
					where
						sv = case conses of
							TH.NormalC name tys : _ -> foldl TH.AppE (TH.ConE name) (map (const $ TH.VarE 'safeValue) tys)
							_ -> error "not a normal constructor for safe value."
				x = TH.mkName "x"
				encodeF = TH.FunD 'encode [TH.Clause [TH.VarP x] (TH.NormalB $ TH.VarE 'undefined) []]
				decodeF = TH.FunD 'decode [TH.Clause [TH.VarP x] (TH.NormalB $ TH.VarE 'undefined) []]
		def i n maxTys name vars cons = do
			let funN = TH.mkName $ "fe"++TH.nameBase conN
			TH.runIO $ putStrLn $ "defining construction for "++show (name, vars, cons)
			case (n, maxTys) of
				-- special cases for enums, they are easy.
				(1,0) -> return [TH.FunD funN [TH.Clause [] (TH.NormalB $ feLow $ constSizedLTE 0 (constLTE 0)) []]]
				(2,0) -> return [TH.FunD funN [TH.Clause [] (TH.NormalB $ feLow $ constSizedLTE 1 (constLTE i)) []]]
				(n,0) -> return [TH.FunD funN [TH.Clause [] (TH.NormalB $ feLow $ constSizedLTE n (constLTE (shiftL (1 :: Int) i))) []]]
			where
				feLow e = TH.ConE 'FELow `TH.AppE` e
				constLTE c = TH.ConE 'LFEConst `TH.AppE` TH.LitE (TH.IntegerL $ fromIntegral c)
				constSizedLTE size e = TH.TupE [TH.LitE $ TH.IntegerL $ fromIntegral size, e]
				conN = case cons of
					TH.NormalC n _ -> n
					_ -> error $ "Only normal constructors are allowed. Problem is "++show (TH.ppr cons)

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
		feTuple = TH.ConT ''FE `TH.AppT` tupleType
		_matchHeaderCode = TH.FunD '_matchHeader
			[TH.Clause [tupleP] (TH.NormalB $ TH.DoE stmts) []]
			where
				stmts = zipWith (\name ix -> TH.NoBindS $ TH.VarE '_setReady `TH.AppE` TH.VarE name `TH.AppE` TH.LitE (TH.IntegerL ix))
					names [0..]
		match = TH.InstanceD (map matchPred fes) (matchC tupleFEs) [_matchHeaderCode]
		change = TH.InstanceD (map changePred fes) (changeC tupleFEs) []
		tupleNName = TH.mkName $ "tuple"++show n
		tupleNTy = TH.SigD tupleNName $ TH.ForallT (map TH.PlainTV names) (map bitReprPred types) $ TH.ArrowT `TH.AppT` tupleFEs `TH.AppT` feTuple
		tupleN = TH.FunD tupleNName [TH.Clause [] (TH.NormalB $ TH.VarE 'tuple) []]
		x = TH.mkName "x"
		tuplePFELow = TH.TupP $ map (\n -> TH.ConP 'FELow [TH.VarP n]) names
		tupleF = TH.FunD 'tuple
			[TH.Clause [tuplePFELow]
				(TH.NormalB $ TH.ConE 'FELow `TH.AppE` TH.TupE [sumSize, cat])
				[] ]
			where
				list = TH.ListE (map TH.VarE names)
				sumSize = TH.VarE 'sum `TH.AppE` (TH.VarE 'map `TH.AppE` TH.VarE 'fst `TH.AppE` list)
				cat = TH.ConE 'LFECat `TH.AppE` list
		tupleD = TH.InstanceD [] (tupleC tupleFEs)
			[ TH.TySynInstD ''FETuple [tupleFEs] feTuple
			, tupleF
			]
		safeValueV = TH.FunD 'safeValue [TH.Clause [] (TH.NormalB $ TH.TupE $ map (const $ TH.VarE 'safeValue) names) []]
		plusT a b = TH.ConT ''Plus `TH.AppT` a `TH.AppT` b
		encodeF = TH.FunD 'encode
			[TH.Clause [tupleP]
				(TH.NormalB $ TH.VarE '_concatenateSizedBitVectors `TH.AppE`
					TH.ListE (map (\v -> TH.TupE [TH.VarE 'bitReprSize `TH.AppE` v, TH.VarE 'encode `TH.AppE` v]) $ map TH.VarE names)) []]
		decodeF = TH.FunD 'encode [TH.Clause [TH.VarP x] (TH.NormalB $ TH.VarE r) [rD, ssD]]
			where
				ssD = TH.ValD (TH.VarP ss) (TH.NormalB $ TH.ListE $ map (\n -> TH.VarE 'bitReprSize `TH.AppE` TH.VarE n) names) []
				ss = TH.mkName "ss"
				rD = TH.ValD (TH.AsP r tupleP) (TH.NormalB $ TH.VarE 'undefined) []
				r = TH.mkName "r"
		bitSize t = TH.ConT ''BitSize `TH.AppT` t
		bitReprD = TH.InstanceD (TH.ClassP ''Nat [bitSize tupleType]:map bitReprPred types) (bitReprC tupleType)
			[ encodeF
			, TH.FunD 'decode [TH.Clause [TH.VarP x] (TH.NormalB $ TH.VarE 'undefined) []]
			, safeValueV
			, TH.TySynInstD ''BitSize [tupleType] $ foldl plusT (bitSize $ head types) (map bitSize $ tail types)
			]
	in do
		let rs = [match, change, tupleD, bitReprD, tupleNTy, tupleN]
		when (n == 3) $ TH.runIO $ mapM_ (print . TH.ppr) rs
		return rs
 )

