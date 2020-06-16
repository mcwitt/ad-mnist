{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MNIST.Classifier where

import Control.Monad.Random
import Data.Proxy
import Data.Singletons
import Data.Singletons.TypeLits
import MNIST
import Numeric.LinearAlgebra.Static
import Prelude hiding ((<>))

-- | Feature matrix. Number of examples (m) by number of features (n)
data Examples :: * where
  MkExamples :: (KnownNat m, KnownNat n) => L m n -> Examples

type Labels m = R m

data Classifier n h1 k
  = -- | Classifier using 'n' features with 'h1' hidden units and 'k' outputs
    Classifier
      { w1 :: L n h1,
        b1 :: R h1,
        w2 :: L h1 k,
        b2 :: R k
      }

initRandom :: (MonadRandom m, KnownNat n, KnownNat h1, KnownNat k) => m (Classifier n h1 k)
initRandom = Classifier <$> w1 <*> b1 <*> w2 <*> b2
  where
    w1 = rand_ xavier
    b1 = rand_ gaussian
    w2 = rand_ xavier
    b2 = rand_ gaussian
    rand_ :: MonadRandom m => (Seed -> a) -> m a
    rand_ f = do
      s :: Int <- getRandom
      return (f s)

initOnes :: (KnownNat n, KnownNat h1, KnownNat k) => Classifier n h1 k
initOnes =
  Classifier
    { w1 = matOnes,
      b1 = vecOnes,
      w2 = matOnes,
      b2 = vecOnes
    }

matOnes :: forall m n. (KnownNat m, KnownNat n) => L m n
matOnes = build (const (const 1))

vecOnes :: forall n. KnownNat n => R n
vecOnes = let l = fromIntegral (natVal (Proxy :: Proxy n)) in vector (replicate l 1)

xavier :: forall n1 n2. (KnownNat n1, KnownNat n2) => Seed -> L n1 n2
xavier s = uniformSample s (- c) c
  where
    c = sqrt (6 / (n1_ + n2_))
    n1_ = fromIntegral . natVal $ (Proxy :: Proxy n1)
    n2_ = fromIntegral . natVal $ (Proxy :: Proxy n2)

gaussian :: forall n. KnownNat n => Seed -> R n
gaussian s = randomVector s Gaussian

forward ::
  (KnownNat m, KnownNat n, KnownNat h1, KnownNat k) =>
  Classifier n h1 k ->
  L m n ->
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
w +~ b = w + vecOnes `outer` b

classifier ::
  (KnownNat m, KnownNat n, KnownNat p) =>
  -- | training examples
  L m n ->
  -- | training labels
  R n ->
  -- | prediction examples
  L p n ->
  -- | prediction labels
  R p
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
