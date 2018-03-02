module Ftap.Report (ftapReport) where

import Ftap.TestRun (Test, resultOf)

import Prelude ((<>), ($), show)

import Data.Array (length, (..), zipWith)
import Data.Either (Either(..))
import Data.String (joinWith)


ftapReport :: Array Test -> String
ftapReport results = 
  "1.." <> (show $ length results) <> "\n" <> (joinWith "\n" zipResultsWithInts)
  where
    zipResultsWithInts = zipWith toStringReport (1..(length results)) results
    toStringReport i t = (isOk t) <> " " <> (show i) <> " " <> (show t)
    isOk t = case (resultOf t) of
        Left _ -> "not ok"
        Right _ -> "ok"
