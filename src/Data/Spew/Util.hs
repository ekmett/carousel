module Data.Spew.Util where

-- ceil (a / b)
cdiv :: Integral a => a -> a -> a
cdiv a b = div (a + b - 1) b
