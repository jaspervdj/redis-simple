-- | This module is meant to make working with redis in Haskell more simple. It
-- is a small layer above the full-blown @redis@ package.
--
-- It only supports a small subset of the redis features.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Database.Redis.Simple
    ( -- * Type for keys
      Key (..)

      -- * Working with simple key-value pairs
    , itemGet
    , itemExists
    , itemSet
    , itemDelete

      -- * Working with sets
    , setAdd
    , setRemove
    , setContains
    , setFindAll
    , setRandMember

      -- * Working with lists
    , listRightPush
    , listIndex
    ) where

import           Data.Binary          (Binary, decode, encode)
import           Data.ByteString      (ByteString)
import           Data.Maybe           (catMaybes)
import           GHC.Exts             (IsString)

import           Database.Redis.Redis

-- | Type for a key in the key-value store
--
newtype Key = Key {unKey :: ByteString}
            deriving (Show, Eq, Ord, IsString, Semigroup, Monoid, Binary)

-- | Gets an item from the database
--
itemGet :: Binary a
        => Redis         -- ^ Redis handle
        -> Key           -- ^ Key of the value to get
        -> IO (Maybe a)  -- ^ Resulting value
itemGet redis (Key key) = do
    reply <- get redis key
    return $ case reply of RBulk (Just r) -> Just $ decode r
                           _              -> Nothing

-- | Checks if an item with a given key exists
--
itemExists :: Redis    -- ^ Redis handle
           -> Key      -- ^ Key to test
           -> IO Bool  -- ^ If the key exists
itemExists redis (Key key) = do
    reply <- exists redis key
    return $ case reply of RInt 1 -> True
                           _      -> False

-- | Set an item in the database
--
itemSet :: Binary a
        => Redis     -- ^ Redis handle
        -> Key       -- ^ Key
        -> a         -- ^ Value
        -> IO ()     -- ^ No result
itemSet redis (Key key) item = do
    _ <- set redis key (encode item)
    return ()

-- | Delete an item in the database
--
itemDelete :: Redis  -- ^ Redis handle
           -> Key    -- ^ Key
           -> IO ()  -- ^ No result
itemDelete redis (Key key) = do
    _ <- del redis key
    return ()

-- | Add an item to a redis set
--
setAdd :: Binary a
       => Redis     -- ^ Redis handle
       -> Key       -- ^ Key of the set
       -> a         -- ^ Item to add to the set
       -> IO ()     -- ^ No result
setAdd redis (Key s) m = do
    _ <- sadd redis s $ encode m
    return ()

-- | Remove an item from a redis set
--
setRemove :: Binary a
          => Redis     -- ^ Redis handle
          -> Key       -- ^ Key of the set
          -> a         -- ^ Item to remove from the set
          -> IO ()     -- ^ No result
setRemove redis (Key s) m = do
    _ <- srem redis s $ encode m
    return ()

-- | Check if a set contains a certain item
--
setContains :: Binary a
            => Redis     -- ^ Redis handle
            -> Key       -- ^ Key of the set
            -> a         -- ^ Item to check for
            -> IO Bool   -- ^ If the item is present in the set
setContains redis (Key s) m = do
    reply <- sismember redis s $ encode m
    return $ case reply of
        RInt 1 -> True
        _      -> False

-- | Get all items from a set
--
setFindAll :: Binary a
           => Redis     -- ^ Redis handle
           -> Key       -- ^ Key of the set
           -> IO [a]    -- ^ All items in the set
setFindAll redis (Key s) = do
    reply <- smembers redis s
    case reply of
        RMulti (Just replies) ->
            return $ catMaybes $ flip map replies $ \r -> case r of
                RBulk i -> decode <$> i
                _       -> Nothing
        _ -> return []


-- | Get a random member from a set
--
setRandMember :: Binary a
              => Redis         -- ^ Redis handle
              -> Key           -- ^ Key of the set
              -> IO (Maybe a)  -- ^ A random member from the set
setRandMember redis (Key s) = do
    reply <- srandmember redis s
    return $ case reply of RBulk (Just r) -> Just $ decode r
                           _              -> Nothing


-- | Right push an item to a redis list
--
listRightPush :: Binary a
              => Redis     -- ^ Redis handle
              -> Key       -- ^ Key of the list
              -> a         -- ^ Item to push to the list
              -> IO ()     -- ^ No result
listRightPush redis (Key s) m = do
    _ <- rpush redis s $ encode m
    return ()


listIndex :: Binary a
          => Redis         -- ^ Redis handle
          -> Key           -- ^ Key of the list
          -> Int           -- ^ Index
          -> IO (Maybe a)  -- ^ Resulting value
listIndex redis (Key key) idx = do
    reply <- lindex redis key idx
    return $ case reply of RBulk (Just r) -> Just $ decode r
                           _              -> Nothing

