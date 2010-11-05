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
tests = [ testCase "redis-simple itemSet"       itemSetTest
        , testCase "redis-simple itemGet"       itemGetTest
        , testCase "redis-simple itemExists"    itemExistsTest
        , testCase "redis-simple setAdd"        setAddTest
--        , testCase "redis-simple listRightPush" listRightPushTest
--        , testCase "redis-simple itemDelete"    deleteTest
--        , testCase "redis-simple itemIsNoMore"  itemIsNoMoreTest
        ]


------------------------------------------------------------------------------
-- N.B. These tests are simple checks for basically successful behaviour,
--      but cannot be comprehensive, since that would require testing the 
--      underlying library. Grano salis.


testing :: String
testing = "ἐστίν"


testKey :: Key
testKey = Key $ U.fromString "single"


------------------------------------------------------------------------------
itemSetTest :: H.Assertion
itemSetTest = do
    con <- connect "127.0.0.1" defaultPort
    _ <- select con 0
    returning <- itemSet con testKey testing
    disconnect con
    H.assertEqual "Set" () returning


------------------------------------------------------------------------------
itemGetTest :: H.Assertion
itemGetTest = do
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


------------------------------------------------------------------------------
listRightPushTest :: H.Assertion
listRightPushTest = do
    con <- connect "127.0.0.1" defaultPort
    _ <- select con 0
    _ <- listRightPush con testKey testing
    returning <- listIndex con testKey 0
    disconnect con
    H.assertEqual "listRightPush (RPUSH)" testing (fromJust returning)

-}

------------------------------------------------------------------------------
setAddTest :: H.Assertion
setAddTest = do
    con <- connect "127.0.0.1" defaultPort
    _ <- select con 0
    returning <- setAdd con testKey testing
    disconnect con
    H.assertEqual "setAdd (SADD)" () returning


