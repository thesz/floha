-- |Floha.hs
--
-- Top-level module for data flow hardware description language along the lines
-- of CAPH.
--
-- Copyright (C) 2013 Serguey Zefirov

{-# LANGUAGE GADTs, TypeOperators, TypeFamilies #-}

module Language.Floha where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

infixr 5 :.
data a :. b = a :. b
data Nil = Nil

data VarID = VarID (Maybe String) Int
	deriving (Eq, Ord, Show)

-- |Floha expression structure.
data FE a where
	FConst :: a -> FE a
	FVar :: VarID -> FE a

-- |Lift HList to the HList of Floha expressions.
type family LiftFE ts
type instance LiftFE Nil = Nil
type instance LiftFE (t :. ts) = FE t :. LiftFE ts

-- |Floha actor.
data Actor ins outs where
	-- |Actor is either real actor - a state machine.
	Actor :: LiftFE ins -> LiftFE outs -> Actor ins outs
	-- |Or actor is a network of connections between actors.
	Network :: Actor ins outs

