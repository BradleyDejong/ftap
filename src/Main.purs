module Main where

import Ftap.Report (ftapReport)
import Ftap.TestRun (Test(..))

import Prelude
import Data.Either (Either(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ ftapReport
    [ Test "blank" (Right unit)
    , Test "hi test" (Right unit)
    , Test "fail test" (Left "There was a crazy big failure")
    , ToDo "todo test" (Right unit)
    , Skip "skip test" (Right unit)
    , Test "equal test" (equal 1 2)
    ]



equal :: forall a. Eq a => Show a => a -> a -> Either String Unit
equal x y
  | x == y = Right unit
  | otherwise = Left ("expected " <> (show x) <> " to equal " <> (show y))

-- notEqual :: forall a. Eq a => Show a => a -> a -> Test
