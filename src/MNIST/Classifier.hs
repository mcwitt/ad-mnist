{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MNIST.Classifier where

import Data.Singletons
import Data.Singletons.Prelude.Tuple
import Data.Singletons.TypeLits
import GHC.TypeLits
import MNIST
import Numeric.LinearAlgebra.Static
import Prelude hiding ((<>))

-- | Feature matrix. Number of examples (m) by number of features (n)
data Examples :: * where
  MkExamples :: Features m n -> Examples

type Features m n = L m n

type Labels m = R m

data Classifier n h1 k
  = -- | Classifier using 'n' features with 'h1' hidden units and 'k' outputs
    Classifier
      { w1 :: L n h1,
        b1 :: R h1,
        w2 :: L h1 k,
        b2 :: R k
      }

initial :: (KnownNat n, KnownNat h1, KnownNat k) => Classifier n h1 k
initial =
  Classifier
    { w1 = matrix [1 ..],
      b1 = vector [1 ..],
      w2 = matrix [1 ..],
      b2 = vector [1 ..]
    }

forward ::
  (KnownNat m, KnownNat n, KnownNat h1, KnownNat k) =>
  Classifier n h1 k ->
  Features m n ->
  L m k
forward Classifier {..} xs =
  let a1 = dmmap relu $ xs <> w1 +~ b1
      af = dmmap relu $ a1 <> w2 +~ b2
   in af
  where
    relu x
      | x > 0 = x
      | otherwise = 0

-- | Matrix-vector sum broadcasting over the first index
(+~) :: (KnownNat m, KnownNat n) => L m n -> R n -> L m n
w +~ b = w + vector [1 ..] `outer` b

classifier ::
  (KnownNat m, KnownNat n, KnownNat p) =>
  -- | training examples
  Features m n ->
  -- | training labels
  Labels n ->
  -- | prediction examples
  Features p n ->
  -- | prediction labels
  Labels p
classifier = undefined

fromImages :: Images -> Examples
fromImages Images {..} =
  case ( toSing (fromIntegral _numImages),
         toSing (fromIntegral imageSize)
       ) of
    ( SomeSing (SNat :: Sing m),
      SomeSing (SNat :: Sing n)
      ) ->
        MkExamples (matrix (fmap fromIntegral _pixels) :: L m n)
  where
    imageSize = _numRows * _numCols
