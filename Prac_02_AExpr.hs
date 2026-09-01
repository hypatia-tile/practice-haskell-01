{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Prac_02_AExpr where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Functor (($>))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word8
import TestKit (runCases, runTests)

data Expr
  = Lit Integer
  | Neg Expr
  | Bin Op Expr Expr
  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

data Cursor = Cursor {pos :: !Int}
  deriving (Show)

{- | ラベルからバイトが一意に決まるトークン、つまり約物。

「そのバイトかどうか」しか判定しないので、'token' は @()@ を返せばよい。
複数のバイトを受け入れるもの (数字・空白) はここに入れず、
'digit' \/ 'space' として独立させてある。
-}
data TokenLabel = LParen | RParen | Minus | Plus | Star | Slash
  deriving (Eq, Show, Ord)

{- | エラーが運ぶ「期待したもの」。増やすのは自由。

宣言順がそのまま表示順になる ('Ord' が導出、'Set.toList' が昇順)。
-}
data Label = Tok TokenLabel | Number | EndOfInput | Named String
  deriving (Eq, Show, Ord)

renderLabel :: Label -> String
renderLabel l = case l of
  Tok LParen -> sym "("
  Tok RParen -> sym ")"
  Tok Minus -> sym "-"
  Tok Plus -> sym "+"
  Tok Star -> sym "*"
  Tok Slash -> sym "/"
  Number -> term "number"
  EndOfInput -> term "end of input"
  Named s -> term s
 where
  sym = ("'" <>) . (<> "'")
  term = ("<" <>) . (<> ">")

data ParseError
  = Unexpected {errPos :: Int, expected :: Set Label}
  deriving (Eq, Show)

newtype Parser a = P {runParser :: ByteString -> Cursor -> Either ParseError (a, Cursor)}

instance Functor Parser where
  fmap f (P p) = P \src cur -> fmap (\(a, s) -> (f a, s)) $ p src cur

instance Applicative Parser where
  pure a = P \_ cur -> pure (a, cur)
  (P p) <*> (P q) = P \src cur -> do
    (f, cur') <- p src cur
    (q', cur'') <- q src cur'
    pure (f q', cur'')

instance Monad Parser where
  (P p) >>= f = P \src cur -> do
    (p', cur') <- p src cur
    runParser (f p') src cur'

instance Alternative Parser where
  empty = P \_ cur -> Left $ Unexpected (pos cur) Set.empty
  (P p) <|> (P q) = P \src cur -> case p src cur of
    Right a -> Right a
    Left lhs@(Unexpected pos1 want1) -> case q src cur of
      Right b -> Right b
      Left rhs@(Unexpected pos2 want2) -> Left $
        case compare pos1 pos2 of
          LT -> rhs
          EQ -> Unexpected pos1 (want1 <> want2)
          GT -> lhs

{- | 1バイトを述語で判定して読み進める。__位置を進めるのはこの関数だけ__。

失敗時のラベル集合を呼び出し側が渡す。入力末尾と述語不一致を区別しないのは、
どちらも「ここに @labels@ のどれかが欲しかった」で言い尽くせるため。
不足しているバイトの正体は @src@ とオフセットから復元できるので、
エラー値に持たせる必要が無い。

位置を進める場所がここ1箇所であることは、'Alternative' の
「より先まで進んだ失敗が勝つ」併合が成立する前提になっている。
-}
core :: Set Label -> (Word8 -> Bool) -> Parser Word8
core labels cond = P \src cur ->
  let off = pos cur
   in case src BS.!? off of
        Just b | cond b -> Right (b, cur{pos = off + 1})
        _ -> Left $ Unexpected off labels

{- | 約物1文字。空白は消費しないので、文法からは 'symbol' 経由で使う。

@()@ を返すのは、どのバイトが来たかが引数の 'TokenLabel' から既に分かるため。
述語が複数のバイトを受け入れる 'core' \/ 'digit' \/ 'space' と違い、
呼び出し側の知らない情報がここには無い。
-}
token :: TokenLabel -> Parser ()
token label =
  let one c = () <$ core (Set.singleton (Tok label)) (== c)
   in case label of
        LParen -> one _parenleft
        RParen -> one _parenright
        Minus -> one _hyphen
        Plus -> one _plus
        Star -> one _asterisk
        Slash -> one _slash

{- | 数字1文字。数を組み立てるのはこれの繰り返し (@some digit@)。

'token' と違いバイトを返すのは、10通りのどれが来たかがラベルから決まらないため。
-}
digit :: Parser Word8
digit = core (Set.singleton Number) isDigit

{- | 空白1文字。ラベルが空集合なので expecting に現れない。

空白は文法上の意味を持つ単位ではなく、「空白が必要です」と言われても
利用者が直せる情報にならないため、期待値としては何も主張しない。
その代わり単体で失敗すると expecting が空のエラーになるので、
@many@ \/ @optional@ の下でのみ使うこと。

バイトを返すのは述語が複数のバイトを受け入れるから (改行を区別したくなったときに残る)。
-}
space :: Parser Word8
space = core Set.empty isSpace

-- | 入力の終端。位置を進めないので、'core' の不変条件には触れない。
eof :: Parser ()
eof = P \src cur -> case src BS.!? pos cur of
  Nothing -> pure ((), cur)
  Just _ -> Left $ Unexpected (pos cur) (Set.singleton EndOfInput)

{- | 本体の直後の空白を消費して「レキシム」にする。

空白をトークンの前で消費するか後ろで消費するかは決めの問題だが、
__後ろに統一する__と「コンビネーターの境目では常に実トークンの先頭にいる」
という不変条件が手に入る。@<|>@ は両辺を同じ位置から試すので、
前で消費する規約だと選択肢の数だけ同じ空白を読み直すことになる。
'eof' も特別扱いが要らない (末尾の空白は直前のレキシムが食べている)。

代償は入力__先頭__の空白で、これだけは誰も消費しない。
入口が @many space *>@ で一度だけ落とすこと。
-}
lexeme :: Parser a -> Parser a
lexeme p = p <* many space

-- | 約物のレキシム。文法が空白に触れずに済むよう、これを使う。
symbol :: TokenLabel -> Parser ()
symbol = lexeme . token
