{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LATS.Matrix
  ( Matrix
  , rows
  , cols
  , fromRows
  , toRows
  , fromColumns
  , toColumns
  , tr
  , (<.>)
  , (#>)
  , (<#)
  , (#)
  , inv
  , pinv ) where

import qualified Numeric.LinearAlgebra as L
import LATS.Vector
import LATS.Matrix.Raw ()

import Data.Indexed
import Control.Newtype
import Control.ConstraintClasses

instance HasIndexed L.Matrix where
  newtype Indexed L.Matrix i a = Matrix (L.Matrix a)
  mkIx m = Matrix m
  unIx (Matrix m) = m

type Matrix i j = Indexed L.Matrix (i,j)
type List i = Indexed [] i

type instance CKey (Matrix i j) = (i,j)

instance (Newtype i Int, Newtype j Int) => Newtype (i,j) (Int,Int) where
  pack (!i,!j) = (pack i, pack j)
  unpack (!i,!j) = (unpack i, unpack j)

----------------------------------------------------------------------

rows :: Matrix i j a -> Int
rows = L.rows . unIx

cols :: Matrix i j a -> Int
cols = L.cols . unIx

fromRows :: L.Element a => List i (Vector j a) -> Matrix i j a
fromRows = mkIx . L.fromRows . fmap unIx . unIx

toRows :: L.Element a => Matrix i j a -> List i (Vector j a)
toRows = mkIx . map mkIx . L.toRows . unIx

fromColumns :: L.Element a => List j (Vector i a) -> Matrix i j a
fromColumns = mkIx . L.fromColumns . fmap unIx . unIx

toColumns :: L.Element a => Matrix i j a -> List j (Vector i a)
toColumns = mkIx . map mkIx . L.toColumns . unIx

tr :: (L.Transposable (L.Matrix a) (L.Matrix a))
   => Matrix i j a -> Matrix j i a
tr = mkIx . L.tr' . unIx

----------------------------------------------------------------------

infixr 8 <.>
(<.>) :: L.Numeric a => Vector i a -> Vector i a -> a
v1 <.> v2 = unIx v1 L.<.> unIx v2

infixr 8 #>
(#>) :: L.Numeric a => Matrix i j a -> Vector j a -> Vector i a
m #> v = mkIx (unIx m L.#> unIx v)

infixl 8 <#
(<#) :: L.Numeric a => Vector i a -> Matrix i j a -> Vector j a
v <# m = mkIx (unIx v L.<# unIx m)

infixr 8 #
(#) :: L.Numeric a => Matrix i j a -> Matrix j k a -> Matrix i k a
m1 # m2 = mkIx (unIx m1 L.<> unIx m2)

inv :: L.Field a => Matrix i j a -> Matrix j i a
inv = mkIx . L.inv . unIx

pinv :: L.Field a => Matrix i j a -> Matrix j i a
pinv = mkIx . L.pinv . unIx
