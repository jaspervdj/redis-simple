module Database.Redis.Simple.Tests
    ( tests ) where

------------------------------------------------------------------------------
-- import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as U
-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy.Char8 as L
-- import           Data.Maybe
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
-- import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as H
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Monadic

import           Database.Redis.Redis
import           Database.Redis.Simple


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCase "redis-simple set-ok"        setOkTest
        , testCase "redis-simple get"           getTest
        , testCase "redis-simple itemExists"    itemExistsTest
--        , testCase "redis-simple itemDelete"    deleteTest
--        , testCase "redis-simple itemIsNoMore"  itemIsNoMoreTest
        ]

testing :: String
testing = "ἐστίν"


testKey :: Key
testKey = Key $ U.fromString "single"

------------------------------------------------------------------------------
setOkTest :: H.Assertion
setOkTest = do
    con <- connect "127.0.0.1" defaultPort 
    _ <- select con 0
    returning <- itemSet con testKey testing
    disconnect con
    H.assertEqual "Set" () returning

------------------------------------------------------------------------------
getTest :: H.Assertion
getTest = do
    con <- connect "127.0.0.1" defaultPort 
    _ <- select con 0
    Just returning <- itemGet con testKey
    disconnect con
    H.assertEqual "Get should match set" testing returning


------------------------------------------------------------------------------
itemExistsTest :: H.Assertion
itemExistsTest = do
    con <- connect "127.0.0.1" defaultPort 
    _ <- select con 0
    returning <- itemExists con testKey
    disconnect con
    H.assertEqual "Item set should exist" True returning

{-
------------------------------------------------------------------------------
deleteTest :: H.Assertion
deleteTest = do
    con <- connect "127.0.0.1" defaultPort 
    _ <- select con 0
    returning <- itemDelete con testKey
    disconnect con
    H.assertEqual "Deletion" () returning


------------------------------------------------------------------------------
itemIsNoMoreTest :: H.Assertion
itemIsNoMoreTest = do
    con <- connect "127.0.0.1" defaultPort 
    _ <- select con 0
    returning <- itemExists con testKey
    disconnect con
    H.assertEqual "Item set should no longer exist" False returning
-}
