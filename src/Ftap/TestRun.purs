module Ftap.TestRun (TestResult(..), resultOf, Test(..)) where

import Prelude (Unit, (<>), (<<<), map, (-), ($))
import Data.String (split, Pattern(Pattern), joinWith)
import Data.Show (class Show, show)
import Data.Either

import Control.Monad.Eff

data Test
  = Test String (forall eff. Eff eff (Either String Unit))
  | Skip String (forall eff. Eff eff (Either String Unit))
  | ToDo String (forall eff. Eff eff (Either String Unit))
  
--TODO write monad instance for Test
    
data TestResult =
  Tested String (Either String Unit)
  | Skipped String (Either String Unit)
  | ToDone String (Either String Unit)

instance showTest :: Show TestResult where
    show (Tested name res) = name <> (message' res)
    show (Skipped name res) = name <> " # skip" <> (message' res)
    show (ToDone name res) = name <> " # TODO" <> (message' res)

message' :: Either String Unit -> String
message' (Left msg) = " " <> errorMsg msg
message' (Right _) = ""

errorMsg :: String -> String
errorMsg msg = "\n" <> msg'
  where
    msg' = indent 1 ("---\nmessage: '" <> msg <> "'\n...")


    
resultOf :: TestResult -> Either String Unit
resultOf t = case t of
  Tested _ r -> r
  Skipped _ r -> r
  ToDone _ r -> r


indent :: Int -> String -> String
indent = indentWith "  "

indentWith :: String -> Int -> String -> String
indentWith p n = indentWith' "" p n

indentWith' :: String -> String -> Int -> String -> String
indentWith' soFar prefix 0 = joinWith "\n" <<< map (\s -> soFar <> s) <<< split (Pattern "\n")
indentWith' soFar prefix n = indentWith' (soFar <> prefix) prefix (n - 1)
