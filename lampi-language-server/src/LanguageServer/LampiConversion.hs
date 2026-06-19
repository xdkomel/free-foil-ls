{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE QuantifiedConstraints #-}

module LanguageServer.LampiConversion where

-- import qualified Lampi.Abs as R
-- import qualified Lampi.Print as R
-- import LanguageServer.Lampi

-- TODO: conversion between BNFC-generated Lampi AST and LanguageServer.Lampi types
-- (see LanguageServer.FlanConversion for reference)
