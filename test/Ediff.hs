{-# language QuasiQuotes #-}
module Ediff where

import Data.Tagged
import System.IO.Temp
import System.Process

import Test.Tasty.Options

newtype Ediff = Ediff Bool

instance IsOption Ediff where
  defaultValue = Ediff False
  parseValue _ = Just $ Ediff True
  optionName = Tagged "ediff"
  optionHelp = Tagged ""

ediff :: String -> String -> String -> IO ()
ediff testName a b = do
  a' <- writeSystemTempFile (testName ++ ".expected") a
  b' <- writeSystemTempFile (testName ++ ".received") b
  let quote s = "\"" ++ s ++ "\""
  callCommand $ "emacsclient"
    ++ " --create-frame"
    -- ++ " --no-wait"
    ++ " --alternate-editor\"\""
    ++ " --eval \'(ediff-files " ++ quote a' ++ quote b' ++ ")\'"
    ++ " &"

