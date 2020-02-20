{-# Language BlockArguments #-}
{-# Language RankNTypes #-}
{-# Language PatternGuards #-}
module Galois.Matrix
  ( Matrix(..)
  , ident
  , mul
  , submatrix
  , inv
  , vandermonde
  , rowmatrix
  , vec
  , row
  , rows, cols
  ) where

import Control.Exception.Base (Exception, throw)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import qualified Data.Vector as V (Vector, MVector)
import qualified Data.Vector.Generic as V hiding (Vector)
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as SV
import Control.Loop (numLoop, numLoopFold)
import Galois.Field

type Row = SV.Vector G

-- | A row-major 'Matrix' of G entries
-- TODO: columns should be stored so we can have a 0 row matrix
-- TODO: allow 0 padding at the end of the individual rows, (ragged right margins) only really affects the zipWiths
-- this would enable us to not have to 0 pad in the carve function
data Matrix = Matrix {-# UNPACK #-} !Int {-# UNPACK #-} !(V.Vector Row)
  deriving Show

matrixSize :: Matrix -> (Int, Int)
matrixSize (Matrix cs m) = (V.length m, cs)

ident :: Int -> Matrix
ident n = Matrix n $ V.generate n \i -> V.slice (n-i) n v where -- uses one backing vector
  v = V.generate (n+n) \i -> if i == n then G 1 else G 0
  -- V.generate n \i -> V.generate n \j -> if i == j then G 1 else G 0

data DimensionMismatch = DimensionMismatch String deriving (Show, Eq)
instance Exception DimensionMismatch

dimensionMismatch :: String -> Matrix -> Matrix -> DimensionMismatch
dimensionMismatch f m1 m2 = DimensionMismatch message where
  message = unwords
    [ "Can't", f
    , "matrix of size", show (matrixSize m1)
    , "with matrix of size", show (matrixSize m2)
    ]

cols :: Matrix -> Int
cols (Matrix cs _) = cs

rows :: Matrix -> Int
rows (Matrix _ m) = V.length m

at :: Matrix -> Int -> Int -> G
at (Matrix _ m) i j = V.unsafeIndex (V.unsafeIndex m i) j

instance Semigroup Matrix where
  (<>) = mul

generate :: Int -> Int -> (Int -> Int -> G) -> Matrix
generate rs cs f = Matrix cs $ V.generate rs \i -> V.generate cs \j -> f i j

mul :: Matrix -> Matrix -> Matrix
mul m n
  | y /= rows n = throw $ dimensionMismatch "multiply" m n
  | otherwise = Matrix z $
    V.generate x \i ->
      V.generate z \k ->
        numLoopFold 0 (y-1) 0 \acc j -> acc + at m i j * at n j k
  where
    x = rows m
    y = cols m
    z = cols n

vec :: Matrix -> V.Vector Row
vec (Matrix _ v) = v

augment :: Matrix -> Matrix -> Matrix
augment m n
  | rows m /= rows n = throw $ dimensionMismatch "augment" m n
  | otherwise = Matrix (cols m + cols n) $ V.zipWith (V.++) (vec m) (vec n)

submatrix :: Matrix -> Int -> Int -> Int -> Int -> Matrix
submatrix (Matrix cs m) rmin cmin rmax cmax
  | cmin == 0, cmax == cs = Matrix cs m' 
  | cs' <- cmax - cmin = Matrix cs' $ V.slice cmin cs' <$> m'
  where m' = V.slice rmin (rmax - rmin) m

swapRows :: V.MVector s a -> Int -> Int -> ST s ()
swapRows = MV.swap

isSquare :: Matrix -> Bool
isSquare m = rows m == cols m

data SingularMatrix = SingularMatrix deriving (Show, Eq)
instance Exception SingularMatrix

inv :: Matrix -> Matrix
inv m@(Matrix n _)
  | not (isSquare m) = throw $ DimensionMismatch $ unwords
    [ "Can't invert non-square matrix of size", show (matrixSize m) ]
  | otherwise = submatrix (gaussianElimination (augment m (ident n))) 0 n n (n * 2)

modify :: (forall s. V.MVector s (SV.Vector G) -> ST s ()) -> Matrix -> Matrix
modify f m = Matrix (cols m) $ V.modify f (vec m) where

gaussianElimination :: Matrix -> Matrix
gaussianElimination = modify \ m -> do
  let n = MV.length m

  numLoop 0 (n - 1) \r -> do
    mrr <- load m r r
    when (mrr == G 0) do
      let go i = when (i < n) $
            load m i r >>= \scale ->
              if scale /= 0
              then swapRows m r i
              else go (i+1)
      go (r+1)

    mrr' <- load m r r
    when (mrr' == G 0) $ unsafeIOToST $ throw SingularMatrix

    when (mrr' /= G 1) do
      let scale = recip mrr'
      mr <- MV.read m r
      MV.write m r $ V.map (*scale) mr

    when (n > r + 1) $ numLoop (r + 1) (n - 1) \i -> do
      scale <- load m i r
      when (scale /= G 0) do
        mr <- MV.read m r
        mi <- MV.read m i
        MV.write m i $ V.zipWith (+) mi $ V.map (scale*) mr

  numLoop 0 (n - 1) \d ->
    when (d > 0) $ numLoop 0 (d - 1) \i -> do
      scale <- load m i d
      when (scale /= G 0) do
        mi <- MV.read m i
        md <- MV.read m d
        MV.write m i $ V.zipWith (+) mi $ V.map (scale *) md
  where
    load :: V.MVector s Row -> Int -> Int -> ST s G
    load m i j = do
      r <- MV.read m i
      V.indexM r j

vandermonde :: Int -> Int -> Matrix
vandermonde rs cs = generate rs cs \i j -> X i ^ j -- G (i+1) ^ j would be a little cheaper, but a different order

rowmatrix :: Matrix -> Int -> Matrix
rowmatrix (Matrix cs m) i = Matrix cs $ V.singleton (m V.! i)

row :: Matrix -> Int -> Row
row (Matrix _ m) i = m V.! i
