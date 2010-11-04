module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Database.Redis.Simple.Tests
main :: IO ()
main = defaultMain tests
  where tests = [
                  testGroup "Database.Redis.Simple.Tests"
                            Database.Redis.Simple.Tests.tests
                ]
