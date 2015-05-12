{-# LANGUAGE MonadComprehensions #-}

module Main where

import Data.Maybe
import Data.Monoid

main :: IO ()
main = mapM_ (putStrLn . fizzbuzz) [1..100]
  where
    fizzbuzz x = fromMaybe (show x) $ ["Fizz" | x `rem` 3 == 0] <>
                                      ["Buzz" | x `rem` 5 == 0]
