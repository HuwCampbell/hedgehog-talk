{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Hedgehog.Main (defaultMain)
import qualified Test.CircularBuffer.Report
import qualified Test.CircularBuffer.Hedgehog


main :: IO ()
main =
  defaultMain [
    Test.CircularBuffer.Report.tests
  , Test.CircularBuffer.Hedgehog.tests
  ]
