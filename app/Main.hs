module Main where

import MNIST (load)
import qualified MNIST
import MNIST.Classifier
import Numeric.LinearAlgebra.Static

main :: IO ()
main = do
  images <- load MNIST.images (dataDir ++ "train-images")
  labels <- load MNIST.labels (dataDir ++ "train-labels")
  print images
  print labels
  let examples = fromImages images
  case examples of
    MkExamples fs -> print (size fs)
  where
    dataDir = "data/result/"
