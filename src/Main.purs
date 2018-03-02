module Main where

import Ftap.Report (ftapReport)
import Ftap.TestRun (TestResult(..))

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe (Maybe(Just, Nothing))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ ftapReport [
    Ok Nothing
    , Ok (Just "HI")
    , NotOk (Just "failure tests") "There was a crazy big failure"
    ]


