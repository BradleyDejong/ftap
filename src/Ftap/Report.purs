module Ftap.Report (ftapReport) where

import Ftap.TestRun (TestResult(..))

import Prelude ((<>), ($), show, (<<<), map, (-))

import Data.String (split, Pattern(Pattern), joinWith)
import Data.Array (length, (..), zipWith)
import Data.Maybe (Maybe(Just, Nothing))


indent :: Int -> String -> String
indent = indentWith "  "

indentWith :: String -> Int -> String -> String
indentWith p n = indentWith' "" p n

indentWith' :: String -> String -> Int -> String -> String
indentWith' soFar prefix 0 = joinWith "\n" <<< map (\s -> soFar <> s) <<< split (Pattern "\n")
indentWith' soFar prefix n = indentWith' (soFar <> prefix) prefix (n - 1)


ok :: Int -> Maybe String -> String
ok idx msg =
  "ok " <> (show idx) <> case msg of
    Nothing -> ""
    Just s -> " " <> s


notOk :: Int -> Maybe String -> String -> String
notOk idx desc msg = "not ok " <> (show idx) <> desc' <> "\n" <> msg'
  where
    desc' = case desc of
      Nothing -> ""
      Just s -> " " <> s
    msg' = indent 1 ("---\nmessage: '" <> msg <> "'\n...")


ftapReport :: Array TestResult -> String
ftapReport results = 
  "1.." <> (show $ length results) <> "\n" <> (joinWith "\n" zipResultsWithInts)
  where
    zipResultsWithInts = zipWith toStringReport (1..(length results)) results
    toStringReport i r = case r of
        Ok m -> ok i m
        NotOk m e -> notOk i m e
