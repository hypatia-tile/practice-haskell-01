{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Writer.CPS
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word8

testInput :: (Show a) => Scanner a -> IO ()
testInput scanner = do
  print (runWriter $ runStateT scanner input)

input :: ScannerState
input = initScannerState "hello;wo;rld;"

initScannerState :: ByteString -> ScannerState
initScannerState source = S source 0 0
{-# INLINE initScannerState #-}

data Span = Span
  { start :: Int
  , len :: Int
  }
  deriving (Show)

data ScannerState = S
  { source :: ByteString
  , anchor :: Int
  , cursor :: Int
  }
  deriving (Show)

pAdvance :: ScannerState -> ScannerState
pAdvance (S sr s e) = S sr s (e + 1)
{-# INLINE pAdvance #-}

pCurrent :: ScannerState -> Maybe Word8
pCurrent (S sr _ e) = sr BS.!? e
{-# INLINE pCurrent #-}

pSync :: ScannerState -> ScannerState
pSync (S sr _ e) = (S sr e e)
{-# INLINE pSync #-}

pScan :: ScannerState -> Span
pScan (S _ s e) = Span s (e - s)
{-# INLINE pScan #-}

type Scanner = StateT ScannerState (Writer [Span])

advance :: Scanner (Maybe Word8)
advance = do
  x <- gets pCurrent
  modify pAdvance
  return x

peek :: Scanner (Maybe Word8)
peek = gets pCurrent

untilMatch :: (Word8 -> Bool) -> Scanner Bool
untilMatch p = do
  x <- peek
  case fmap p x of
    Just False -> do
      advance
      untilMatch p
    Just True -> return True
    Nothing -> return False

skip :: Scanner ()
skip = modify pAdvance

scan :: Scanner ()
scan = do
  hitSep <- untilMatch (== _semicolon)
  gets pScan >>= tell . (: [])
  when hitSep $ do
    skip
    modify pSync
    scan

test :: IO ()
test = testInput scan
