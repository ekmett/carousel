{-# Language BlockArguments #-}
{-# Language RankNTypes #-}
{-# Language PatternGuards #-}
module Matrix
  ( Matrix(..)
  , Vec
  , ident
  , mul
  , submatrix
  , inv
  , generate
--  , vandermonde
  , rowmatrix
  , vec
  , row
  , rows, cols
  ) where

import Control.Exception.Base (Exception, throw)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV hiding (Vector)
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Storable as SV
import Control.Loop (numLoop, numLoopFold)
import Field

type Vec = SV.Vector F

-- | A row-major 'Matrix' of G entries
-- TODO: columns should be stored so we can have a 0 row matrix
-- TODO: allow 0 padding at the end of the individual rows, (ragged right margins) only really affects the zipWiths
-- this would enable us to not have to 0 pad in the carve function
data Matrix = Matrix {-# UNPACK #-} !Int {-# UNPACK #-} !(V.Vector Vec)
  deriving Show

matrixSize :: Matrix -> (Int, Int)
matrixSize (Matrix cs m) = (V.length m, cs)

ident :: Int -> Matrix
ident n = Matrix n $ GV.generate n \i -> SV.slice (n-i) n v where -- uses one backing vector
  v = SV.generate (n+n) \i -> if i == n then 1 else 0

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

at :: Matrix -> Int -> Int -> F
at (Matrix _ m) i j = SV.unsafeIndex (V.unsafeIndex m i) j

instance Semigroup Matrix where
  (<>) = mul

generate :: Int -> Int -> (Int -> Int -> F) -> Matrix
generate rs cs f = Matrix cs $ V.generate rs \i -> SV.generate cs \j -> f i j

mul :: Matrix -> Matrix -> Matrix
mul m n
  | y /= rows n = throw $ dimensionMismatch "multiply" m n
  | otherwise = Matrix z $
    V.generate x \i ->
      SV.generate z \k ->
        numLoopFold 0 (y-1) 0 \acc j -> acc + at m i j * at n j k
  where
    x = rows m
    y = cols m
    z = cols n

vec :: Matrix -> V.Vector Vec
vec (Matrix _ v) = v

augment :: Matrix -> Matrix -> Matrix
augment m n
  | rows m /= rows n = throw $ dimensionMismatch "augment" m n
  | otherwise = Matrix (cols m + cols n) $ V.zipWith (SV.++) (vec m) (vec n)

submatrix :: Matrix -> Int -> Int -> Int -> Int -> Matrix
submatrix (Matrix cs m) rmin cmin rmax cmax
  | cmin < 0 = error "submatrix column starts < 0"
  | cmin > cmax = error "submatrix starts after it ends"
  | cmin > cs = error "submatrix column starts > end"
  | rmin < 0 = error "submatrix row starts < 0"
  | rmin > rmax = error "submatrix row starts after it ends"
  | rmin > V.length m = error "submatrix row starts > end"
  | cmin == 0, cmax == cs = Matrix cs m' 
  | cs' <- cmax - cmin = Matrix cs' $ SV.slice cmin cs' <$> m'
  where m' = V.slice rmin (rmax - rmin) m

isSquare :: Matrix -> Bool
isSquare m = rows m == cols m

data SingularMatrix = SingularMatrix deriving (Show, Eq)
instance Exception SingularMatrix

inv :: Matrix -> Matrix
inv m@(Matrix n _)
  | not (isSquare m) = throw $ DimensionMismatch $ unwords
    [ "Can't invert non-square matrix of size", show (matrixSize m) ]
  | otherwise = submatrix (gaussianElimination (augment m (ident n))) 0 n n (n * 2)

modify :: (forall s. V.MVector s Vec -> ST s ()) -> Matrix -> Matrix
modify f m = Matrix (cols m) $ V.modify f (vec m) where

gaussianElimination :: Matrix -> Matrix
gaussianElimination = modify \ m -> do
  let n = GMV.length m

  numLoop 0 (n - 1) \r -> do
    mrr <- load m r r
    when (mrr == 0) do
      let go i = when (i < n) $
            load m i r >>= \scale ->
              if scale /= 0
              then GMV.swap m r i
              else go (i+1)
      go (r+1)

    mrr' <- load m r r
    when (mrr' == 0) $ unsafeIOToST $ throw SingularMatrix

    when (mrr' /= 1) do
      let scale = recip mrr'
      mr <- GMV.read m r
      GMV.write m r $ SV.map (*scale) mr

    when (n > r + 1) $ numLoop (r + 1) (n - 1) \i -> do
      scale <- load m i r
      when (scale /= 0) do
        mr <- GMV.read m r
        mi <- GMV.read m i
        GMV.write m i $ SV.zipWith (+) mi $ SV.map (scale*) mr

  numLoop 0 (n - 1) \d ->
    when (d > 0) $ numLoop 0 (d - 1) \i -> do
      scale <- load m i d
      when (scale /= 0) do
        mi <- GMV.read m i
        md <- GMV.read m d
        GMV.write m i $ SV.zipWith (+) mi $ SV.map (scale *) md
  where
    load :: V.MVector s Vec -> Int -> Int -> ST s F
    load m i j = do
      r <- GMV.read m i
      SV.indexM r j


rowmatrix :: Matrix -> Int -> Matrix
rowmatrix (Matrix cs m) i = Matrix cs $ V.singleton (m V.! i)

row :: Matrix -> Int -> Vec
row (Matrix _ m) i = m V.! i
