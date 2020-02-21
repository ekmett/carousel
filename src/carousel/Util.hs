module Util
  ( cdiv
  ) where

cdiv :: Integral a => a -> a -> a
cdiv a b = div (a - 1) b + 1
