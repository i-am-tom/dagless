{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-|
Module      : Data.Tuple.Morph
Description : An interface for tuple manipulations using HLists.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Shout out http://hackage.haskell.org/package/tuple-morph - I'll be dropping
this file and adding the dependency as soon as I've proven the concept and had
time to PR the library for 8.6.*.
-}
module Data.Tuple.Morph where

import Data.HList          (HList (..))
import Data.Kind           (Type)
import Data.Tuple.Morph.TH (makeMorphInstance)

-- | Convert between @Tuple@ and 'HList'. This allows us to manipulate tuples
-- in a much more intuitive and "regular" way.
--
-- >>> :t fromTuple ("hello", True)
-- fromTuple ("hello", True) :: HList '[[Char], Bool]
--
-- >>> :t toTuple ("hello" :> True :> HNil)
-- toTuple ("hello" :> True :> HNil) :: ([Char], Bool)

class Morph (types :: [Type]) (tuple :: Type)
    | types -> tuple, tuple -> types where
  fromTuple :: tuple -> HList types
  toTuple   :: HList types -> tuple

$(traverse makeMorphInstance [2 .. 62])
