-- |Base.hs
--
-- Base definitions for Floha.
--
-- Copyright (C) 2013 Serguey Zefirov.

{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, PatternGuards, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, StandaloneDeriving #-}

module Language.Floha.Base
	( (:.)(..)
	, Nil(..)
	, BitRepr(..)
	, FE(..)
	, (-->)
	, LiftFE
	, Tuple(..)
	, Reset(..)
	, Clock(..)
	, Actor(..)
	, actor
	, actorN
	, ActorBody
	, auto
	, autoN
	, initial
	, __
	, rules
	, rulesND
	, constant
	, report
	, simulate
	, Logic(..)
	, AddSub(..)
	, net
	, feFalse
	, feTrue
	, feUnit
	, deriveBitRepr
	, generateCode
	, Language(..)
	) where

import Control.Monad
import Control.Monad.State

import Data.Bits

import Data.Int
import qualified Data.Map as Map
import Data.Maybe
import Data.List(intercalate, nub)
import qualified Data.Set as Set
import Data.Word
import Text.Printf

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

type Twice a = Plus a a

type family Max a b
type instance Max Z (S b) = S b
type instance Max (S a) Z = S a
type instance Max (S a) (S b) = S (Max a b)

newtype NatI = NatI { fromNatI :: Integer}
	deriving (Eq, Ord, Num)

instance Show NatI where
	show = show . fromNatI

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

-- |Kind of variable.
data VarK = Var | Ready | Valid
	deriving (Eq, Ord, Show)

-- |How we identify variables.
data VarID = VarID (Maybe String) [Int] VarK
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
	-- regular constant.
		LFEConst	BitVectConst
	-- variable - input, output, register, temp...
	|	LFEVar		VarID
	-- wildarg. Can be pattern and value.
	|	LFEWild
	-- Various binary operations.
	|	LFEBin		LFWBinOp	SizedLFE	SizedLFE
	-- Concatenation of expressions, basically curly brackets from Verilog.
	|	LFECat		[SizedLFE]
	-- LFESub value high low: take subvector (low..high) from value. Tuple splits use this. Indices are inclusive.
	-- The size should be high-low+1.
	|	LFESub		SizedLFE	Int	Int
	-- Change size from one to another. Change to smaller size basically is the same as LFESub v (size-1) 0.
	|	LFEChangeSize	ChangeFiller	SizedLFE
	-- The register assignment. Arguments are name of the clock, default value (after reset) and the computed value.
	|	LFERegister	String	BitVectConst	SizedLFE
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

-- |Tuple support.
class Tuple tup where
	-- |Transformation at type level - from (FE a, FE b) to FE (a,b)
	type FETuple tup

	-- |The code for transformation.
	tuple :: tup -> FETuple tup

-- |Reset signal properties.
class Reset r where
	-- |Name of reset signal.
	resetName :: r -> String

	-- |Reset active polarity, True for positive (1).
	resetActiveHigh :: r -> Bool

	-- |Is reset synchronous?
	resetSynchronous :: r -> Bool

class (Reset (ClockReset c), Show c) => Clock c where
	-- |The type of clock reset signal.
	type ClockReset c

	-- |The clock name.
	clockName :: c -> String

	-- |Whether clock edge is positive (front).
	clockPosEdge :: c -> Bool

	-- |Reset type for this clock.
	clockReset :: c -> ClockReset c

	-- |Clock frequency.
	clockFrequency :: c -> Rational

data ClockInfo = ClockInfo {
	  ciClockName		:: String
	, ciResetName		:: String
	, ciClockPosEdge	:: Bool
	, ciResetSync		:: Bool
	, ciResetActiveHigh	:: Bool
	}
	deriving (Eq, Ord, Show)

clockInfo :: Clock c => c -> ClockInfo
clockInfo c = ClockInfo {
	  ciClockName		= clockName c
	, ciResetName		= resetName r
	, ciClockPosEdge	= clockPosEdge c
	, ciResetSync		= resetSynchronous r
	, ciResetActiveHigh	= resetActiveHigh r
	}
	where
		r = clockReset c

-- |Default reset type for default clock.
data DefaultReset = DefaultReset
	deriving (Eq, Ord, Show)

instance Reset DefaultReset where
	resetName = show
	resetActiveHigh = const False
	resetSynchronous = const False

data DefaultClock = DefaultClock
	deriving (Eq, Ord, Show)

instance Clock DefaultClock where
	type ClockReset DefaultClock = DefaultReset
	clockName = show
	clockPosEdge = const True
	clockReset = const DefaultReset
	clockFrequency = const 1

data ClockHolder where
	ClockHolder :: Clock c => c -> ClockHolder

data RulesType = Deterministic | NonDeterministic
	deriving (Eq, Ord, Show)

data Rules = Rules RulesType [SizedLFE] [SizedLFE] [([SizedLFE], [SizedLFE])]
	deriving (Eq, Ord, Show)

-- |Instance of an actor. Name of instance, clocked inputs and outputs and the internal representation of an actor.
data Instance = Instance String [(ClockInfo, SizedLFE)] [(ClockInfo, SizedLFE)] LLActor
	deriving (Eq, Ord, Show)

-- |FIFO annotation. What channel FIFO is added to and the size.
data FIFO = FIFO SizedLFE Int
	deriving (Eq, Ord, Show)

type ClockedSizedLFE = (ClockInfo, SizedLFE)

-- |Low level representation of Floha actor.
data LLActor =
	-- state machine variant of actor.
	-- includes: name, inputs, outputs, assignments for the variables, matching rules (at least one) and clock information.
		LLActor	String [SizedLFE] [SizedLFE] (Map.Map VarID SizedLFE) [Rules] ClockInfo
	-- network variant of actor.
	-- includes: name, inputs and outputs with associated clock information, instances of actors and FIFO annotations
	-- for channels.
	|	LLNet String [ClockedSizedLFE] [ClockedSizedLFE] [Instance] [FIFO]
	deriving (Eq, Ord, Show)

-- |Floha actor.
data Actor ins outs where
	Actor :: LLActor -> Actor ins outs

deriving instance Show (Actor ins outs)

-------------------------------------------------------------------------------
-- Main combinators.

actor :: (FEList (LiftFE ins), FEList (LiftFE outs)) => String -> ActorBody (LiftFE ins) (LiftFE outs) -> Actor ins outs
actor name body = _mkActor name Nothing body

actorN :: (FEList (LiftFE ins), FEList (LiftFE outs)) => String -> StringList (LiftFE ins) -> ActorBody (LiftFE ins) (LiftFE outs) -> Actor ins outs
actorN name names body = _mkActor name (Just names) body

net :: String -> Actor ins outs
net name = error "net is not yet ready."

-- |Deterministic version of rules - it does not take into account readyness of the outputs.
rules :: (Match matchE, Change changeE) => (matchE, changeE) -> [(matchE, changeE)] -> ActorBodyM ()
rules (headerMatch, headerResults) matchChanges = do
	let r = Rules Deterministic (matchExpressions headerMatch) (changeExpressions headerResults)
		(map (\(m,c) -> (matchExpressions m, changeExpressions c)) matchChanges)
	modify $ \abs -> abs { absRules = r : absRules abs }

-- |Non-deterministic rules - rule considered firing if patterns matches and valid and outputs are ready.
rulesND :: (Match matchE, Change changeE) => (matchE, changeE) -> [(matchE, changeE)] -> ActorBodyM ()
rulesND (headerMatch, headerResults) matchChanges = do
	let r = Rules NonDeterministic (matchExpressions headerMatch) (changeExpressions headerResults)
		(map (\(m,c) -> (matchExpressions m, changeExpressions c)) matchChanges)
	modify $ \abs -> abs { absRules = r : absRules abs }

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

-- |Display the value, if condition is met.
report :: (BitRepr a, Show a) => FE Bool -> FE a -> ActorBodyM ()
report condition value = do
	error "report !!!"

frequency :: Clock c => ActorBodyM Rational
frequency = do
	ClockHolder c <- liftM absClock get
	return $ clockFrequency c

-------------------------------------------------------------------------------
-- Implementation.

data ABState = ABS {
	-- |Generating unique indices.
	  absUnique		:: Int
	-- |Initial values for the variables.
	, absInitials		:: Map.Map VarID SizedLFE
	-- |Register signals as inputs.
	, absInputs		:: [SizedLFE]
	-- |And also register signals as outputs.
	, absOutputs		:: [SizedLFE]
	-- |Rules.
	, absRules		:: [Rules]
	-- |Clock specification.
	, absClock		:: ClockHolder
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
	, absInputs		= []
	, absOutputs		= []
	, absRules		= []
	, absClock		= ClockHolder DefaultClock
	}

_unique :: ActorBodyM Int
_unique = modify (\abs -> abs { absUnique = absUnique abs + 1 }) >> liftM absUnique get

_inventVarID :: Maybe String -> ActorBodyM VarID
_inventVarID name = liftM (($ Var) . VarID name . (\n -> [n])) _unique

_setInitial :: VarID -> SizedLFE -> ActorBodyM ()
_setInitial v init = modify $ \abs -> abs { absInitials = Map.insert v init $ absInitials abs }

cloneVarID :: VarID -> ActorBodyM VarID
cloneVarID (VarID n ns k) = do
	suffix <- _unique
	return $ VarID n (ns ++ [suffix]) k

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

class FEList feList where
	inventList :: Maybe (StringList feList) -> ActorBodyM feList
	_toSizedLFEs :: feList -> [SizedLFE]

instance FEList Nil where
	inventList _ = return Nil
	_toSizedLFEs = const []

instance (BitRepr a, FEList feList) => FEList (FE a :. feList) where
	inventList names = do
		fe <- inventVar name
		feList <- inventList names'
		return $ fe :. feList
		where
			(name, names') = case names of
				Just (n :. ns) -> (Just n, Just ns)
				Nothing -> (Nothing, Nothing)
	_toSizedLFEs (FELow slfe :. list) = slfe : _toSizedLFEs list


class Match match where
	-- |Convert the match into the list of expressions.
	matchExpressions :: match -> [SizedLFE]

class Change change where
	-- |Convert the change part into the list of expressions.
	changeExpressions :: change -> [SizedLFE]

encodeConst :: BitRepr a => a -> SizedLFE
encodeConst a = (bitReprSize a, LFEConst $ encode a)

internal :: String -> a
internal s = error $ "internal error: "++s

lfeTrue, lfeFalse :: SizedLFE
[lfeTrue, lfeFalse] = map encodeConst [True, False]

lfeReady, lfeValid :: VarID -> SizedLFE
lfeReady (VarID n ns k) = (1, LFEVar (VarID n ns Ready))
lfeValid (VarID n ns k) = (1, LFEVar (VarID n ns Valid))

lfeEqual, lfeAnd :: SizedLFE -> SizedLFE -> SizedLFE
lfeEqual a@(s, _) b = (s, LFEBin Equal a b)
lfeAnd a b = (fst a, LFEBin And a b)

instance Match (FE a) where
	matchExpressions (FELow sizedLFE) = [sizedLFE]

instance Change (FE a) where
	changeExpressions (FELow sizedLFE) = [sizedLFE]

_mkActor :: forall feIns feOuts ins outs . (feIns ~ LiftFE ins, feOuts ~ LiftFE outs, FEList feIns, FEList feOuts) => String -> Maybe (StringList feIns) -> ActorBody feIns feOuts -> Actor ins outs
_mkActor name names body = flip evalState startABState $ do
	ins <- inventList names :: ActorBodyM (LiftFE ins)
	outs <- body (ins :: LiftFE ins)
	rules <- liftM absRules get
	ClockHolder c <- liftM absClock get
	initials <- liftM absInitials get
	let insLFEs = _toSizedLFEs ins
	    outsLFEs = _toSizedLFEs outs
	when (nonUnique insLFEs) $ error $ "Input names are not unique in "++name
	when (nonUnique outsLFEs) $ error $ "Output names are not unique in "++name
	let renameIO (LFEVar v@(VarID (Just n) ns k)) = Map.singleton v (VarID (Just n) (tail ns) k)
	    renameIO v = Map.empty
	let ioRenames = Map.unions $ map renameIO $ map snd $ insLFEs ++ outsLFEs
	rules <- forM rules $ renameRulesVars ioRenames
	return $ toActor ioRenames ins outs rules c initials
	where
		renameAny :: Map.Map VarID VarID -> SizedLFE -> SizedLFE
		renameAny rmap (sz, e) = (,) sz $ case e of
			LFEVar v -> case Map.lookup v rmap of
				Just v -> LFEVar v
				Nothing -> e
			LFEBin op a b -> LFEBin op (renameAny rmap a) (renameAny rmap b)
			LFECat es -> LFECat $ map (renameAny rmap) es
			LFESub a o1 o2 -> LFESub (renameAny rmap a) o1 o2
			LFEChangeSize filler a -> LFEChangeSize filler (renameAny rmap a)
			LFERegister x y a -> LFERegister x y (renameAny rmap a)

		renameRulesVars :: Map.Map VarID VarID -> Rules -> ActorBodyM Rules
		renameRulesVars renames (Rules rulesType headerFrom headerTo matchChanges) = do
			matchChanges' <- forM matchChanges $ \(p',e') -> do
				let p = map rename p'
				    e = map rename e'
				rs <- liftM Map.unions $ mapM (inventRenames Map.empty) p
				return (map (renameAny rs) p, map (renameAny rs) e)
			return $ Rules rulesType headerFrom' headerTo' matchChanges'
			where
				inventRenames map (_,e) = case e of
					LFEVar v -> liftM (flip (Map.insert v) map) $ cloneVarID v
					LFEBin _ a b -> liftM2 (Map.union) (inventRenames map a) (inventRenames map b)
					LFECat es -> liftM Map.unions $ mapM (inventRenames map) es
					LFESub a _ _ -> inventRenames map a
					LFEChangeSize _ a -> inventRenames map a
					LFERegister _ _ a -> inventRenames map a

				rename = renameAny renames

				headerTo' = map rename headerTo
				headerFrom' = map rename headerFrom
		nonUnique :: [SizedLFE] -> Bool
		nonUnique [] = False
		nonUnique vs
			| all isNothing mbNames = False
			| all isJust mbNames = checkDuplicates (catMaybes mbNames)
			| otherwise = error $ "Not all outputs are named in "++name
			where
				mbNames = map getName vs
				getName (_, LFEVar (VarID v _ _)) = v
				getName _ = internal $ "output is not a variable in "++name
		checkDuplicates (s:ss) = elem s ss || checkDuplicates ss
		checkDuplicates [] = False
		toActor :: (FEList (LiftFE ins), FEList (LiftFE outs), Clock c) => Map.Map VarID VarID -> LiftFE ins -> LiftFE outs -> [Rules] -> c -> Map.Map VarID SizedLFE -> Actor ins outs
		toActor ioRenames ins outs rules c initials = Actor (LLActor name (map (renameAny ioRenames) $ _toSizedLFEs ins) (map checkVar $ map (renameAny ioRenames) $ _toSizedLFEs outs) initials rules (clockInfo c))
		checkVar (sz,LFEVar n) = (sz,LFEVar n)
		checkVar e = error $ "Actor "++show name++": output is not a variable: "++show e

-------------------------------------------------------------------------------
-- Simulate actors.

-- |Describe input sequences.
data Sequence a = Wait Int | Data [a]
	deriving (Eq, Ord, Show)

-- |Transform HList of types into a HList of lists of sequences of types.
type family SeqsList ins
type instance SeqsList Nil = Nil
type instance SeqsList (a :. as) = [Sequence a] :. SeqsList as

-- |Simulate the actor.
simulate :: Actor ins outs -> SeqsList ins -> SeqsList outs
simulate actor seqlist = error "simulate!!!"

-------------------------------------------------------------------------------
-- Code generation.

-- |What language we should generate for.
data Language = VHDL | Verilog
	deriving (Eq, Ord, Show)

-- |Code generation monad internal state.
data CGState = CGState {
	-- |Unique index generator.
	  cgsUnique		:: Int
	-- |Nesting level.
	, cgsNest		:: Int
	-- |Language used throughout generated code.
	, cgsLanguage		:: Language
	-- |Generated lines of code.
	, cgsRevLines		:: [String]
	-- |List of warnings.
	, cgsWarnings		:: [String]
	-- |Current actor being generated (for warnings generation). Includes both original and unique names.
	, cgsCurrentActor	:: (String, String)
	-- |Set of used actor names. We can have idA applied to Bool and Int32 channels, they will be
	-- different actors, actually.
	, cgsUsedActorNames	:: Set.Set String
	-- |Maping between actors and their names.
	, cgsActorsVisited	:: Map.Map LLActor String
	}
	deriving (Eq, Ord, Show)

startCGState :: Language -> CGState
startCGState lang = CGState {
	  cgsUnique		= 0
	, cgsNest		= 0
	, cgsLanguage		= lang
	, cgsRevLines		= []
	, cgsWarnings		= []
	, cgsCurrentActor	= error "no current actor is set."
	, cgsUsedActorNames	= Set.empty
	, cgsActorsVisited	= Map.empty
	}

type CGM a = State CGState a

genLine :: String -> CGM ()
genLine line = modify $
	\cgs -> cgs {
		  cgsRevLines = (if null line then "" else replicate (cgsNest cgs) ' ' ++ line) : cgsRevLines cgs
		}

genNL :: CGM ()
genNL = genLine ""

genLanguage :: CGM Language
genLanguage = liftM cgsLanguage get

genWarning :: String -> CGM ()
genWarning warning = do
	(actorName, uniqueActorName) <- liftM cgsCurrentActor get
	let w = "actor "++show actorName++", unique name "++show uniqueActorName++". warning: "++warning
	modify $ \cgs -> cgs { cgsWarnings = w : cgsWarnings cgs }

genNest :: CGM a -> CGM a
genNest act = do
	n <- liftM cgsNest get
	modify $ \cgs -> cgs { cgsNest = n + 4 }
	r <- act
	modify $ \cgs -> cgs { cgsNest = n }
	return r

genComment :: String -> CGM ()
genComment line = do
	l <- genLanguage
	genLine $ (++" "++line) $ case l of
		VHDL -> "--"
		Verilog -> "//"

actorName :: LLActor -> String
actorName (LLActor name _ _ _ _ _) = name
actorName (LLNet name _ _ _ _) = name

underscoredIndices :: String -> [Int] -> VarK -> String
underscoredIndices name ns k = intercalate "_" (name : map show ns ++ kindStr)
	where
		kindStr = case k of
			Var -> []
			Ready -> ["ready"]
			Valid -> ["valid"]

varIDToStr :: VarID -> String
varIDToStr (VarID s ns k) = underscoredIndices (fromMaybe "generated_var" s) ns k

changeKind :: VarK -> VarID -> VarID
changeKind k (VarID name ns _) = VarID name ns k

genModule :: String -> [ClockedSizedLFE] -> [ClockedSizedLFE] -> [ClockInfo] -> CGM () -> CGM ()
genModule un ins outs clocks compileRules = do
	l <- genLanguage
	case l of
		Verilog -> do
			genLine $ "module "++un
			let cs = map ((,) "input " . (\c -> (1,VarID (Just c) [] Var))) (clks ++ resets)
			let fullInputs = concatMap (signal "input " "output") ins
			let fullOutputs = concatMap (signal "output" "input ") outs
			genNest $ do
				forM_ (zipWith (,) ("(" : repeat ",") (cs++fullInputs ++ fullOutputs)) $ \(comma,(direction,(size, v))) -> do
					if size > 0
						then genLine $ unwords [comma, direction, vlSize size, varIDToStr v]
						else return ()
				genLine ");"
			genNL

		VHDL -> do
			genLine "VHDL QQ"
	compileRules
	case l of
		Verilog -> do
			genNL
			genLine "endmodule"
	return ()
	where
		clks = nub $ map ciClockName clocks
		resets = nub $ map ciResetName clocks
		signal dirTo dirFrom (_,(size, LFEVar vid)) = [(dirTo, (size, vid)), (dirTo, (1, changeKind Valid vid)), (dirFrom, (1, changeKind Ready vid))]
		signal _ _ (_,(_, e)) = internal $ "not a variable: "++show e
		vlSize = take 16 . vlSize'
		vlSize' 1 = repeat ' '
		vlSize' n = printf "[%d:0]" (fromNatI (n-1))++ repeat ' '

genCompileRules :: String -> ClockInfo -> [SizedLFE] -> [SizedLFE] -> Map.Map VarID SizedLFE -> [Rules] -> CGM ()
genCompileRules name ci ins outs initials [] = internal $ "no rules for "++name++"."
genCompileRules name ci ins outs initials [Rules rulesType hLeft hRight matches] = do
	genComment "Rules - single instance."
	
genCompileRules name ci ins outs initials manyRules = do
	internal $ "multiple rules aren't supported. actor "++name

-- |Generate code from actor (either just an actor or network).
generateCode :: Language -> Actor ins outs -> ([String], String)
generateCode lang (Actor actor) = flip evalState (startCGState lang) $ do
	genComment $ "GENERATED CODE!!! DO NOT MODIFY!!!"
	genComment $ ""
	genComment $ "Top level Floha actor: "++actorName actor
	genLine ""
	genLine ""
	gen actor
	lines <- liftM cgsRevLines get
	warns <- liftM cgsWarnings get
	return (reverse warns, unlines $ reverse lines)
	where
		gen a = do
			uniqueName <- findName a
			case uniqueName of
				Nothing -> return ()
				Just un -> do
					oldAN <- liftM cgsCurrentActor get
					modify $ \cgs -> cgs { cgsCurrentActor = (actorName a, un) }
					l <- genLanguage
					code un a
					modify $ \cgs -> cgs { cgsCurrentActor = oldAN }
		code un (LLActor name ins outs initials rules ci) = do
			genComment $ "Actor "++name++"."
			let clocked = zipWith (,) (repeat ci)
			genModule un (clocked ins) (clocked outs) [ci] (genCompileRules name ci ins outs initials rules)
			return ()
		findName a = do
			name <- liftM (Map.lookup a . cgsActorsVisited) get
			case name of
				Just a -> return Nothing
				Nothing -> liftM Just $ inventName $ actorName a
		inventName a = do
			checkInvent a (inventName' 1 a)
		inventName' n a = checkInvent (underscoredIndices a [n] Var) (inventName' (n+1) a)
		checkInvent name cont = do
			registered <- liftM (Set.member name . cgsUsedActorNames) get
			if registered
				then cont
				else do
					modify $ \cgs -> cgs { cgsUsedActorNames = Set.insert name $ cgsUsedActorNames cgs }
					return name


-------------------------------------------------------------------------------
-- Instances of various classes for various types.

instance BitRepr Bool where
	type BitSize Bool = S Z
	safeValue = False
	encode = fromInteger . fromIntegral . fromEnum
	decode = toEnum . fromIntegral . fromBitVectConst

instance BitRepr Word8 where
	type BitSize Word8 = Twice (Twice (Twice (S Z)))
	safeValue = 0
	encode = fromInteger . fromIntegral
	decode = fromIntegral . fromBitVectConst

feFalse :: FE Bool
feFalse = FELow (1, LFEConst 0)

feTrue :: FE Bool
feTrue = FELow (1, LFEConst 1)

instance BitRepr () where
	type BitSize () = Z
	safeValue = ()
	encode = const 0
	decode = const ()

feUnit :: FE ()
feUnit = FELow (0, LFEConst 0)

instance Logic Bool where
	(.&) = (&&)
	(.|) = (||)
	a .^ b = (a .& not b) .| (not a .& b)

-------------------------------------------------------------------------------
-- Derivation of BitRepr for algebraic types.

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
		matchExpressionsCode = TH.FunD 'matchExpressions
			[TH.Clause [tupleP] (TH.NormalB $ foldl1 (\a b -> TH.InfixE (Just a) (TH.VarE '(++)) (Just b)) $ map ((TH.VarE 'matchExpressions `TH.AppE` ) .  TH.VarE) names) []]
		match = TH.InstanceD (map matchPred fes) (matchC tupleFEs) [matchExpressionsCode]
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

