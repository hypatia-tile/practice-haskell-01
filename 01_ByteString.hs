{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word8

semicolon :: Word8
semicolon = 0x3B

testInput :: Show a => Scanner a -> IO ()
testInput scanner = do
  print (runWriter $ runStateT scanner input)

input :: ScannerState
input = initScannerState "hello;wo;rld"

initScannerState :: ByteString -> ScannerState
initScannerState source = S source 0 0 (BS.length source)
{-# INLINE initScannerState #-}

data Span = Span
    { start :: Int
    , len :: Int
    }
    deriving (Show)

data ScannerState = S
    { source :: ByteString
    , start :: Int
    , end :: Int
    , length :: Int
    }
    deriving (Show)

pAdvance :: ScannerState -> ScannerState
pAdvance (S sr s e l) = S sr s (e + 1) l
{-# INLINE pAdvance #-}

pCurrent :: ScannerState -> Maybe Word8
pCurrent (S sr s e l) = sr BS.!? e
{-# INLINE pCurrent #-}

pSync :: ScannerState -> ScannerState
pSync (S sr s e l) = (S sr e e l)
{-# INLINE pSync #-}

pScan :: ScannerState -> Span
pScan (S sr s e l) = Span s (e - s)
{-# INLINE pScan #-}

type Scanner = StateT ScannerState (Writer (Vector Span))

advance :: Scanner (Maybe Word8)
advance = do
    x <- gets pCurrent
    modify pAdvance
    return x

peek :: Scanner (Maybe Word8)
peek = gets pCurrent

untilMatch :: (Word8 -> Bool) -> Scanner ()
untilMatch p = do
  x <- peek
  case fmap p x of
    Just False -> do
      advance
      untilMatch p
    _ -> return ()

scan :: Scanner ()
scan = do
  untilMatch (== semicolon)
  i <- gets pScan
  lift (tell $ V.singleton i)
  advance
  modify pSync
  n <- peek
  case n of
    Nothing -> return ()
    Just _ -> scan

test :: IO ()
test = testInput scan

