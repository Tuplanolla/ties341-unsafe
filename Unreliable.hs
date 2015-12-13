{-# LANGUAGE NumDecimals, ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Timeout (timeout)

seconds :: Integral a => a -> a
seconds = (* 1e+6)

sandbox :: IO a -> IO (Maybe a)
sandbox x = catch
  (timeout (seconds 1) x)
  (\ (_ :: SomeException) -> return Nothing)

unreliable :: IO ()
unreliable =
  do threadDelay (seconds 3)
     error "failed"

main :: IO ()
main =
  do sandbox unreliable >>= print
     sandbox (unsafeInterleaveIO unreliable) >>= print
