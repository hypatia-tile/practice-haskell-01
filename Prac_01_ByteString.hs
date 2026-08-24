{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word8

-- | 入力中の一区画。ByteString を切り出さず、位置と長さだけを持つ。
data Span = Span
  { start :: !Int
  , len :: !Int
  }
  deriving (Eq, Show)

-- | 走査位置。anchor は現在の区画の先頭、cursor は読み取り位置。
data Cursor = Cursor
  { source :: !ByteString
  , anchor :: !Int
  , cursor :: !Int
  }
  deriving (Show)

-- | State モナドを手で書いたもの。走査結果は戻り値で返すので Writer は要らない。
newtype Parser a = Parser {runParser :: Cursor -> (a, Cursor)}

instance Functor Parser where
  fmap f (Parser p) = Parser \s ->
    let (a, s') = p s
     in (f a, s')

instance Applicative Parser where
  pure a = Parser \s -> (a, s)
  Parser pf <*> Parser pa = Parser \s ->
    let (f, s') = pf s
        (a, s'') = pa s'
     in (f a, s'')

instance Monad Parser where
  Parser p >>= k = Parser \s ->
    let (a, s') = p s
     in runParser (k a) s'

initCursor :: ByteString -> Cursor
initCursor src = Cursor src 0 0

-- 基本操作 --------------------------------------------------------------

-- | 読み取り位置のバイト。入力を超えていれば Nothing。
peek :: Parser (Maybe Word8)
peek = Parser \s -> (source s BS.!? cursor s, s)

-- | 読み取り位置を1つ進める。
skip :: Parser ()
skip = Parser \s -> ((), s{cursor = cursor s + 1})

-- | anchor から現在位置までを Span として切り出す。位置は動かさない。
mark :: Parser Span
mark = Parser \s -> (Span (anchor s) (cursor s - anchor s), s)

-- | anchor を現在位置まで進め、次の区画を始める。
sync :: Parser ()
sync = Parser \s -> ((), s{anchor = cursor s})

-- 走査 ------------------------------------------------------------------

{- | 述語を満たすバイトに当たるまで進む。
区切りで止まったら True、入力が尽きたら False。この区別が無いと
末尾の区切りの後ろにある空の区画を取りこぼす。
-}
untilMatch :: (Word8 -> Bool) -> Parser Bool
untilMatch p = go
 where
  go = do
    x <- peek
    case x of
      Nothing -> pure False
      Just c
        | p c -> pure True
        | otherwise -> skip >> go

-- | セミコロン区切りで区画に分ける。区切りが n 個なら区画は n+1 個。
scan :: Parser [Span]
scan = do
  hitSep <- untilMatch (== _semicolon)
  sp <- mark
  if hitSep
    then skip >> sync >> ((sp :) <$> scan)
    else pure [sp]

runScanner :: ByteString -> [Span]
runScanner = fst . runParser scan . initCursor

test :: IO ()
test = print (runScanner "hello;wo;rld;")
