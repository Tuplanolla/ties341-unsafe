{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Monad (forM_)
import Foreign
import Foreign.C
import System.Exit (die)
import System.IO.Unsafe (unsafePerformIO)

type Endo a = a -> a

-- There is no CBool, so we assume that sizeof (bool) == 1 and
-- use CUChar instead (which is a very bad idea).

foreign import ccall "findroot" c_findroot ::
  FunPtr (Endo CDouble) -> CDouble -> CDouble ->
  CDouble -> CUInt -> Ptr CDouble -> IO CUChar

foreign import ccall "findroot_d" c_findroot_d ::
  FunPtr (Endo CDouble) -> FunPtr (Endo CDouble) -> CDouble -> CDouble ->
  CDouble -> CUInt -> Ptr CDouble -> IO CUChar

foreign import ccall "wrapper" c_wrapper ::
  Endo CDouble -> IO (FunPtr (Endo CDouble))

wrap :: Endo Double -> Endo CDouble
wrap f (CDouble x) = CDouble (f x)

findRoot :: Endo Double -> (Double, Double) -> Double -> Int -> Maybe Double
findRoot f (a, b) epsilon n =
  unsafePerformIO $
  do fp <- mallocForeignPtr
     withForeignPtr fp $ \ p ->
       do w <- c_wrapper $ wrap f
          CUChar b <- c_findroot
            w (CDouble a) (CDouble b)
            (CDouble epsilon) (CUInt (fromIntegral n)) p
          freeHaskellFunPtr w
          if b /= 0 then
             do CDouble x <- peek p
                return $ Just x else
             return Nothing

findRoot' ::
  Endo Double -> Endo Double -> (Double, Double) ->
  Double -> Int -> Maybe Double
findRoot' f df (a, b) epsilon n =
  unsafePerformIO $
  do fp <- mallocForeignPtr
     withForeignPtr fp $ \ p ->
       do w <- c_wrapper $ wrap f
          dw <- c_wrapper $ wrap df
          CUChar b <- c_findroot_d
            w dw (CDouble a) (CDouble b)
            (CDouble epsilon) (CUInt (fromIntegral n)) p
          freeHaskellFunPtr w
          freeHaskellFunPtr dw
          if b /= 0 then
             do CDouble x <- peek p
                return $ Just x else
             return Nothing

-- These would typically be split into separate modules.

snaf :: Endo Double
snaf x
  | x > 0 = exp (-1 / x)
  | otherwise = 0

dsnaf :: Endo Double
dsnaf x
  | x > 0 =
    let y = 1 / x in
        y * y * exp (-y)
  | otherwise = 0

strans :: Endo Double
strans x =
  let snafx = snaf x in
      snafx / (snafx + snaf (1 - x))

dstrans :: Endo Double
dstrans x =
  let y = 1 - x
      snafx = snaf x
      snafy = snaf y
      dsnafx = dsnaf x
      dsnafy = dsnaf y
      snafxy = snafx + snafy in
      (snafx * dsnafy - snafy * dsnafx) / (snafxy * snafxy)

main :: IO ()
main =
  let f x = x + (strans ((x + 1) / 2) * 2 - 1) * 2
      df x = 1 + dstrans ((x + 1) / 2) * 2 in
      forM_
      [findRoot f (-2, 8) 1e-6 256,
       findRoot' f df (-2, 8) 1e-6 256] $ \ m ->
      case m of
           Just x -> print x
           Nothing -> die "Numerical error."
