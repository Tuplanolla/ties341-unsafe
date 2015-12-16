module Main where

import Control.Monad.Primitive (unsafeInlineIO)
import Data.ByteString.Internal (accursedUnutterablePerformIO, inlinePerformIO)
import Data.IORef
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

-- All of this is very bad.

counter :: IORef Int
counter = unsafePerformIO $ newIORef 1
{-# NOINLINE counter #-}

increment :: IO Int
increment =
  do n <- readIORef counter
     writeIORef counter (n + 1)
     print n
     return n

main :: IO ()
main =
  let xs =
        [unsafePerformIO increment,
         unsafeDupablePerformIO increment,
         unsafeInlineIO increment,
         inlinePerformIO increment,
         accursedUnutterablePerformIO increment] in
      print $ sum xs
