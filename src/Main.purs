module Main where

import Ftap.Report (ftapReport)
import Ftap.TestRun (TestResult(..), Test(..))

import Prelude
import Data.Either (Either(..))
import Data.Traversable (traverse, sequence)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  results <- (runTests tests)
  log $ ftapReport results

tests :: Array Test
tests =
  [ Test "blank" (pure (Right unit))
  , Test "hi test" (pure (Right unit))
  , Test "fail test" (pure (Left "There was a crazy big failure"))
  , ToDo "todo test" (pure (Right unit))
  , Skip "skip test" (pure (Right unit))
  , Test "equal test" (pure (equal 1 2))
  ]


runTests :: Array Test -> forall eff. Eff eff (Array TestResult)
runTests tests = traverse tests 


equal :: forall a. Eq a => Show a => a -> a -> Either String Unit
equal x y
  | x == y = Right unit
  | otherwise = Left ("expected " <> (show x) <> " to equal " <> (show y))

-- notEqual :: forall a. Eq a => Show a => a -> a -> Tested
