module Ftap.TestRun (Test(..), resultOf) where

import Prelude (Unit, (<>), (<<<), map, (-))
import Data.String (split, Pattern(Pattern), joinWith)
import Data.Show (class Show, show)
import Data.Either

data Test =
  Test String (Either String Unit)
  | Skip String (Either String Unit)
  | ToDo String (Either String Unit)

instance showTest :: Show Test where
    show (Test name res) = name <> (message' res)
    show (Skip name res) = name <> " # skip" <> (message' res)
    show (ToDo name res) = name <> " # TODO" <> (message' res)

message' :: Either String Unit -> String
message' (Left msg) = " " <> errorMsg msg
message' (Right _) = ""

errorMsg :: String -> String
errorMsg msg = "\n" <> msg'
  where
    msg' = indent 1 ("---\nmessage: '" <> msg <> "'\n...")



    
resultOf :: Test -> Either String Unit
resultOf t = case t of
  Test _ r -> r
  Skip _ r -> r
  ToDo _ r -> r


indent :: Int -> String -> String
indent = indentWith "  "

indentWith :: String -> Int -> String -> String
indentWith p n = indentWith' "" p n

indentWith' :: String -> String -> Int -> String -> String
indentWith' soFar prefix 0 = joinWith "\n" <<< map (\s -> soFar <> s) <<< split (Pattern "\n")
indentWith' soFar prefix n = indentWith' (soFar <> prefix) prefix (n - 1)
