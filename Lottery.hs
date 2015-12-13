module Main where

import Data.Set
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Random (randomRIO)
import Text.Read (readMaybe)

entropy :: Int -> IO [Int]
entropy k =
  let x = (:) <$> randomRIO (1, k) <*> unsafeInterleaveIO x in
      x

choose :: Ord a => Int -> [a] -> Set a
choose =
  let f ys n _ | n <= 0 = ys
      f ys n (x : xs)
        | x `member` ys = f ys n xs
        | otherwise = f (x `insert` ys) (n - 1) xs
      f _ n _ = error $ "missing at least " ++ show n ++ " options" in
      f empty

main :: IO ()
main =
  do as <- getArgs
     case sequence $ readMaybe <$> as of
          Just [k, n] | k > 0 && n > 0 && n <= k ->
            entropy k >>= print . toAscList . choose n
          _ -> exitFailure
