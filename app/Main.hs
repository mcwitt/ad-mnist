module Main where

import MNIST

main :: IO ()
main = do
  load images (dataDir ++ "train-images") >>= print
  load labels (dataDir ++ "train-labels") >>= print
  where
    dataDir = "data/result/"
