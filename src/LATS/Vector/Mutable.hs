{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LATS.Vector.Mutable
  ( MVector (..)
  ) where

import qualified Data.Vector.Generic.Mutable.Base as G
import qualified Data.Vector.Storable.Mutable as Mutable

newtype MVector i s a = MVector (Mutable.MVector s a)

instance Mutable.Storable a => G.MVector (MVector i) a where
  basicLength (MVector v) =
    G.basicLength v

  basicUnsafeSlice n m (MVector v) =
    MVector $ G.basicUnsafeSlice n m v

  basicOverlaps (MVector v1) (MVector v2) =
    G.basicOverlaps v1 v2

  basicUnsafeNew n =
    MVector <$> G.basicUnsafeNew n

  basicInitialize (MVector v) =
    G.basicInitialize v

  basicUnsafeReplicate n a =
    MVector <$> G.basicUnsafeReplicate n a

  basicUnsafeRead (MVector v) n =
    G.basicUnsafeRead v n

  basicUnsafeWrite (MVector v) n a =
    G.basicUnsafeWrite v n a

  basicClear (MVector v) =
    G.basicClear v

  basicSet (MVector v) a =
    G.basicSet v a

  basicUnsafeCopy (MVector v1) (MVector v2) =
    G.basicUnsafeCopy v1 v2

  basicUnsafeMove (MVector v1) (MVector v2) =
    G.basicUnsafeMove v1 v2

  basicUnsafeGrow (MVector v) n =
    MVector <$> G.basicUnsafeGrow v n
