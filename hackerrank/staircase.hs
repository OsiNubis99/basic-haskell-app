{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Numeric
import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

genStr xs n | n==0 = []
            | otherwise = xs ++ genStr xs (n-1)

staircase a b max | a<max = do
                    putStrLn ((genStr " " b)++(genStr "#" a))
                    staircase (a+1) (b-1) max
                  | otherwise = do
                    putStrLn ((genStr " " b)++(genStr "#" a))

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    putStrLn "Give me a number:"
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int
    staircase 1 (n-1) n
