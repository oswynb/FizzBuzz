{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonadComprehensions #-}

module Main where

import Data.Maybe
import Data.Monoid

data Fizziness = Fizzy
               | Buzzy
               | FizzyAndBuzzy
               | Flat

instance Monoid Fizziness where
  mempty = Flat
  mappend Flat  x             = x
  mappend FizzyAndBuzzy    _  = FizzyAndBuzzy
  mappend Fizzy Fizzy         = Fizzy
  mappend Fizzy Buzzy         = FizzyAndBuzzy
  mappend Buzzy Fizzy         = FizzyAndBuzzy
  mappend Buzzy Buzzy         = Buzzy
  mappend x     y             = mappend y x

data FizzBuzz (a :: Fizziness) where
  Fizz     :: FizzBuzz Fizzy
  Buzz     :: FizzBuzz Buzzy
  FizzBuzz :: FizzBuzz FizzyAndBuzzy
  Sad      :: Int -> FizzBuzz Flat

instance Show (FizzBuzz Fizzy) where
  show Fizz = "Fizz"

instance Show (FizzBuzz Buzzy) where
  show Buzz = "Buzz"

instance Show (FizzBuzz FizzyAndBuzzy) where
  show FizzBuzz = "FizzBuzz"

instance Show (FizzBuzz Flat) where
  show (Sad x) = show x

main :: IO ()
main = mapM_ (putStrLn . conjugateIntegrity) [1..100]

conjugateIntegrity :: Int -> String
conjugateIntegrity x = sublimateFizziness (inferFizziness x) x

sublimateFizziness :: Fizziness -> (Int -> String)
sublimateFizziness Fizzy         = show . const Fizz
sublimateFizziness Buzzy         = show . const Buzz
sublimateFizziness FizzyAndBuzzy = show . const FizzBuzz
sublimateFizziness Flat          = show . Sad

inferFizziness :: Int -> Fizziness
inferFizziness x = fromMaybe Flat $ [Fizzy | x `rem` 3 == 0] <> [Buzzy | x `rem` 5 == 0]
