{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

import MNIST
import MNIST.Simple
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra.Static
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Forward pass" $ do
    it "should handle 1x1 image" $ do
      let td = Images {_numImages = 1, _numRows = 1, _numCols = 1, _pixels = [1]}
      case fromImages td of
        MkExamples (fs :: L m n) ->
          let clf = initOnes :: Classifier n 1 1
              res = forward clf fs :: L m 1
           in extract res `shouldBe` LA.matrix 1 [4]
