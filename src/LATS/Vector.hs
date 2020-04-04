{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LATS.Vector
  ( Vector ) where

import LATS.Vector.Mutable
import LATS.Vector.Raw ()

import qualified Data.Vector.Storable as L
import qualified Data.Vector.Generic as G

import Data.Indexed

--------------------------------------------------------------------------------

instance HasIndexed L.Vector where
  newtype Indexed L.Vector i a = Vector (L.Vector a)
  mkIx m = Vector m
  unIx (Vector m) = m

type Vector i = Indexed L.Vector i

--------------------------------------------------------------------------------

type instance G.Mutable (Vector i) = MVector i

instance L.Storable a => G.Vector (Vector i) a where
  basicUnsafeFreeze (MVector v) = mkIx <$> G.basicUnsafeFreeze v
  basicUnsafeThaw v = MVector <$> G.basicUnsafeThaw (unIx v)
  basicLength = G.basicLength . unIx
  basicUnsafeSlice i j = mkIx . G.basicUnsafeSlice i j . unIx
  basicUnsafeIndexM v i = G.basicUnsafeIndexM (unIx v) i
