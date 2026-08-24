{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}

{- | 練習ファイル共通の、最小限のテストヘルパ。

hspec や tasty を入れないのは、Cabal project を作らない方針 (docs/adr/0001) だと
テストランナーの導入が重いため。表駆動で足りる範囲に留める。
-}
module TestKit where

import Control.Monad (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Exit (exitFailure)

{- | 入力と期待値の組を関数に流し、食い違いを報告する。失敗した件数を返す。
途中で止めないのは、1回の実行でどのケースが壊れたか全部見たいため。
-}
runCases :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> IO Int
runCases name f cases = do
  bad <- newIORef (0 :: Int)
  forM_ cases \(input, want) -> do
    let got = f input
    if got == want
      then putStrLn ("  ok   " <> show input)
      else do
        modifyIORef' bad (+ 1)
        putStrLn ("  FAIL " <> show input)
        putStrLn ("         want: " <> show want)
        putStrLn ("         got:  " <> show got)
  n <- readIORef bad
  putStrLn (name <> ": " <> show (length cases - n) <> "/" <> show (length cases) <> " passed")
  pure n

{- | 複数の runCases をまとめる。1件でも失敗したら exit 1 で終わるので、
make test と pre-commit hook がそのまま失敗を拾える。
-}
runTests :: [IO Int] -> IO ()
runTests groups = do
  ns <- sequence groups
  if sum ns > 0 then exitFailure else putStrLn "all passed"
