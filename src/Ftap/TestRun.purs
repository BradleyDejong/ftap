module Ftap.TestRun (TestResult(..)) where

import Data.Maybe (Maybe)

data TestResult = Ok (Maybe String) | NotOk (Maybe String) String
