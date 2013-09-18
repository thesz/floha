-- |Floha.hs
--
-- Top-level module for data flow hardware description language along the lines
-- of CAPH.
--
-- Copyright (C) 2013 Serguey Zefirov

{-# LANGUAGE GADTs, TypeOperators, TypeFamilies #-}

module Language.Floha
	( module Language.Floha.Base
	) where

import Language.Floha.Base
import Language.Floha.FPrelude