-- |RTPSwitch.hs
--
-- Seamless switching for SMPTE 2022-6 HD-SDI video/audio streams, encoded as 64-bit 10G Ethernet streams.
--
-- Copyright (C) 2013 Serguey Zefirov.

{-# LANGUAGE ScopedTypeVariables #-}

module RTPSwitch where

import Language.Floha

-- |Clock frequency for 64-bit data word 10G streams.
tengClockFrequency :: Integer
tengClockFrequency = 156250000

-- |Heartbeat generation.
-- Heartbeat is generated by output-only circuit. It is parametrized by
-- packet rate.
heartbeat :: (Integer, Integer) -> Actor Nil (() :. Nil)
heartbeat (packetRateNum, packetRateDenom) = actor "heartbeat" $ \Nil -> do
	acc <- auto
	rem <- auto
	out <- auto
	rules ((acc, rem) --> (acc, rem, out))
		[ (constant 0, rem) --> (constant div, rem .+ constant
	where
		tengFreqAdjust = tengClockFrequency * packetRateDenom
		g = gcd tenGFreqAdjust packetRateNum

		div = 