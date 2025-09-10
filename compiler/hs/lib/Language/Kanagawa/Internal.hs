{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Language.Kanagawa.Internal
  ( equating
  , readFileUtf8
  , truncateInt
  , truncateUInt
  , setAt
  , maybeAt
  , clog2
  ) where

import Control.Exception
import Data.Bits
import Math.NumberTheory.Logarithms
import System.IO

equating :: Eq a => (t -> a) -> t -> t -> Bool
equating p x y = p x == p y

readFileUtf8 :: FilePath -> IO String
readFileUtf8 name = do
    h <- openFile name ReadMode
    hSetEncoding h utf8_bom
    s <- hGetContents h
    -- A hack to force reading of the file so that file exceptions can be
    -- handled on `readFileUtf8`.
    _ <- evaluate (length s)
    return s

truncateUInt :: (Bits a1, Integral a2, Num a1) => a2 -> a1 -> a1
truncateUInt n x = x .&. ((1 `shiftL` fromIntegral n) - 1)

truncateInt :: (Bits a1, Integral a2, Num a1) => a2 -> a1 -> a1
truncateInt n x = let w = n - 1 in
  if 0 == (x .&. (1 `shiftL` fromIntegral w))
      then truncateUInt w x
      else -(1 `shiftL` fromIntegral w) + truncateUInt w x

setAt :: Int -> a -> [a] -> [a]
setAt 0 y (_:xs) = y : xs
setAt n y (x:xs) = x : setAt (n-1) y xs
setAt _ _ [] = []

maybeAt :: Int -> [a] -> Maybe a
maybeAt i xs
    | i >= 0 && i < length xs = Just $ xs !! i
    | otherwise = Nothing

clog2 :: Integer -> Int
clog2 n | isPow2 n  = integerLog2 n
        | otherwise = integerLog2 n + 1
  where
    isPow2 x = x .&. (x - 1) == 0
