redis-simple
============

This package is meant to make working with redis in Haskell more simple. It
is a small layer above the full-blown `redis` package.

Note that it only supports a small subset of the redis features.

Usage
-----

A simple example:

    {-# LANGUAGE OverloadedStrings #-}
    module Main where

    import Database.Redis.Redis (connect, localhost, defaultPort)
    import Database.Redis.Simple (itemSet, itemGet)

    main :: IO ()
    main = do
        redis <- connect localhost defaultPort
        itemSet redis "commander" ("Adama" :: String)
        Just commander <- itemGet redis "commander"
        putStrLn $ commander
