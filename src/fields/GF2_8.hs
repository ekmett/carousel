{-# Language MagicHash #-}
{-# Language TemplateHaskell #-}
{-# Language QuasiQuotes #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# Language BlockArguments #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveGeneric #-}

-- | GF(2^8) mod x^8 + x^4 + x^3 + x^2 + x^0
module GF2_8
  ( F(..)
  , glog
  , gexp
  , pattern X
  , gvmul
  , gvfma
  ) where

import Control.Exception (throw, ArithException(DivideByZero))
import Control.Loop (numLoop)
import Control.Monad.Primitive
import Data.Bits
import Data.Hashable
import Data.Ratio
import Data.Vector.Storable as S
import Data.Vector.Storable.Mutable as MS
import Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Foreign.Storable
import GHC.Generics
import GHC.Ptr
import Language.Haskell.TH
import System.IO.Unsafe (unsafePerformIO)

let 
  logs :: U.Vector Word8
  logs = U.fromList [
      255,0,1,25,2,50,26,198,3,223,51,238,27,104,199,75,
      4,100,224,14,52,141,239,129,28,193,105,248,200,8,76,113,
      5,138,101,47,225,36,15,33,53,147,142,218,240,18,130,69,
      29,181,194,125,106,39,249,185,201,154,9,120,77,228,114,166,
      6,191,139,98,102,221,48,253,226,152,37,179,16,145,34,136,
      54,208,148,206,143,150,219,189,241,210,19,92,131,56,70,64,
      30,66,182,163,195,72,126,110,107,58,40,84,250,133,186,61,
      202,94,155,159,10,21,121,43,78,212,229,172,115,243,167,87,
      7,112,192,247,140,128,99,13,103,74,222,237,49,197,254,24,
      227,165,153,119,38,184,180,124,17,68,146,217,35,32,137,46,
      55,63,209,91,149,188,207,205,144,135,151,178,220,252,190,97,
      242,86,211,171,20,42,93,158,132,60,57,83,71,109,65,162,
      31,45,67,216,183,123,164,118,196,23,73,236,127,12,111,246,
      108,161,59,82,41,157,85,170,251,96,134,177,187,204,62,90,
      203,89,95,176,156,169,160,81,11,245,22,235,122,117,44,215,
      79,174,213,233,230,231,173,232,116,214,244,234,168,80,88,175]
  glog :: Word8 -> Int
  glog i = fromIntegral $ logs U.! (fromIntegral i)
  exps :: U.Vector Word8
  exps = U.create do
    out <- MU.new 510
    numLoop 1 255 \i -> do
      let li = glog i
      MU.write out li (fromIntegral i)
      MU.write out (li + 255) (fromIntegral i)
    return out
  gexp :: Int -> Word8
  gexp i = fromIntegral $ exps U.! (fromIntegral i)
  table :: Name -> U.Vector Word8 -> [DecQ]
  table n xs =
    [ sigD n [t|Ptr Word8|]
    , funD n [clause [] (normalB $ appE (conE 'Ptr) (litE $ stringPrimL $ U.toList $ xs)) []]
    ]
  muls :: U.Vector Word8
  muls = U.generate 65536 \ab ->
    let a = fromIntegral (unsafeShiftR ab 8) :: Word8
        b = fromIntegral (ab .&. 255) :: Word8
    in if a == 0 || b == 0 then 0 else gexp (glog a + glog b)
 in sequence $ table (mkName "logs") logs 
            <> table (mkName "exps") exps
            <> table (mkName "muls") muls

pk :: Ptr Word8 -> Int -> Word8
pk p i = unsafePerformIO $ peekElemOff p i

pkM :: PrimMonad m => Ptr Word8 -> Int -> m Word8
pkM p i = unsafeIOToPrim $ peekElemOff p i
    
-- assumes i != 0
glog :: F -> Int
glog (F i) = fromIntegral $ pk logs (fromIntegral i)
{-# INLINE glog #-}

-- assumes 0 <= i <= 510
gexp :: Int -> F
gexp i = F $ pk exps i
{-# INLINE gexp #-}

pattern X :: Int -> F
pattern X i <- (glog -> i) where
  X i = F $ pk (plusPtr exps 255) $ mod i 255
-- (256*a+b) mod 255 = (255*a+a+b) mod 255 = (a+b) mod 255

-- an 8-bit galois field with polynomial 29
newtype F = F Word8
  deriving (Show, Storable, Eq, Hashable, Generic)

-- instance Show F where showsPrec d (X i) = showParen (d > 10) $ showString "X " . showsPrec 11 i

instance Num F where
  F a + F b = F $ xor a b
  F a - F b = F $ xor a b
  F a * F b = F $ pk muls $ unsafeShiftL (fromIntegral a) 8 .|. fromIntegral b
  negate = id
  abs = id
  signum (F i) = F $ signum i
  fromInteger i = F $ if odd i then 1 else 0

instance Fractional F where
  _ / F 0 = throw DivideByZero
  a / b = gexp $ glog a + 255 - glog b
  recip (F 0) = throw DivideByZero
  recip b = gexp $ 255 - glog b
  fromRational r
    | even (denominator r) = F 0 
    | otherwise = fromInteger (numerator r)

-- non-SIMD multiplication

-- | @gvmul o c i@ sets @o := c*i@ in the galois field
gvmul :: PrimMonad m => MS.MVector (PrimState m) F -> F -> S.Vector F -> m ()
gvmul o (F c) i = do
  let mt = plusPtr muls $ 256 * fromIntegral c
  numLoop 0 (MS.length o - 1) \j -> do
    F a <- S.unsafeIndexM i j
    b <- pkM mt $ fromIntegral a
    MS.unsafeWrite o j $ F b

-- | @vfma o c i@ sets @o += c*i@ in the galois field
gvfma :: PrimMonad m => MS.MVector (PrimState m) F -> F -> S.Vector F -> m ()
gvfma o (F c) i = do
  let mt = plusPtr muls $ 256 * fromIntegral c
  numLoop 0 (MS.length o - 1) \j -> do
    F a <- S.unsafeIndexM i j
    b <- pkM mt $ fromIntegral a
    x <- MS.unsafeRead o j
    MS.unsafeWrite o j $ x + F b
