-- |Floha.hs
--
-- Top-level module for data flow hardware description language along the lines
-- of CAPH.
--
-- Copyright (C) 2013 Serguey Zefirov

module Language.Floha where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

