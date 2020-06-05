{-# LANGUAGE RecordWildCards #-}

module MNIST
  ( images,
    labels,
    load,
  )
where

import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

type Label = Int

type Pixel = Int

data Images
  = Images
      { _numImages :: Int,
        _numRows :: Int,
        _numCols :: Int,
        _pixels :: [Pixel]
      }

data Labels
  = Labels
      { _numLabels :: Int,
        _labels :: [Label]
      }

instance Show Images where
  show Images {..} =
    show _numImages
      ++ " images with size ("
      ++ show _numRows
      ++ ","
      ++ show _numCols
      ++ ")"

instance Show Labels where
  show Labels {..} = show _numLabels ++ " labels"

images :: Get Images
images = do
  _ <- getInt32be
  ni <- fromIntegral <$> getInt32be
  nr <- fromIntegral <$> getInt32be
  nc <- fromIntegral <$> getInt32be
  let numPixels = ni * nr * nc
  ps <- replicateM numPixels (fromIntegral <$> getWord8)
  return $ Images ni nr nc ps

labels :: Get Labels
labels = do
  _ <- getInt32be
  nl <- fromIntegral <$> getInt32be
  ls <- replicateM nl (fromIntegral <$> getWord8)
  return $ Labels nl ls

load :: Get a -> FilePath -> IO a
load f path = runGet f <$> BL.readFile path
