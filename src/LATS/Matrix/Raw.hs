{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module LATS.Matrix.Raw
       ( module HMatrix ) where

import Numeric.LinearAlgebra as HMatrix
import Control.ConstraintClasses

type instance Dom Matrix = Num ::*:: Element ::*:: Container Vector

type instance CKey Matrix = (Int,Int)

instance CFunctor Matrix where
  _fmap = cmap
  {-# INLINE _fmap #-}

instance CLookup Matrix where
  _lookup n v = Just (atIndex v n)
  {-# INLINE _lookup #-}

instance CIndexable Matrix where
  _index = atIndex
  {-# INLINE _index #-}
