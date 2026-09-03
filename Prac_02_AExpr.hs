{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Prac_02_AExpr where

import Control.Applicative (Alternative (empty, many, (<|>)), optional)
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
  Number -> angle "number"
  EndOfInput -> angle "end of input"
  Named s -> angle s
 where
  sym = ("'" <>) . (<> "'")
  angle = ("<" <>) . (<> ">")

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

{- | @p@ を中置演算子 @opp@ で1個以上繋げ、__左結合__で畳む。

左結合の文法は本来 @expr = expr op term@ と書きたいが、再帰下降では
書けない。先頭で自分を呼ぶので1バイトも読まずに再帰し、止まらないため。
そこで「1つ読んでから @(演算子, 項)@ の並びを繰り返し読み、左から畳む」に
書き換える。それを部品にしたのがこれ。

演算子側が @Parser Op@ ではなく @Parser (a -> a -> a)@ なのは、
畳み方そのものを呼び出し側に持たせるため。おかげでこの関数は AST を知らない。

> term = chainl1 factor (symbol Star $> Bin Mul <|> symbol Slash $> Bin Div)

== 演算子を消費したら後戻りしない

@optional opp@ が @Just@ を返した後は、@lp@ の失敗を拾う @\<|\>@ が無い。
つまり__演算子を読んだ時点でこの連鎖に踏み込むことが確定する__。
このパーサー唯一のコミット点で、megaparsec の
「入力を消費したらバックトラックしない」を局所的に真似たもの。

失敗を @\<|\>@ で丸ごと包むと、@"1 + "@ で右辺が pos 4 まで進んで失敗した事実が
捨てられ、カーソルが pos 2 に戻る。__何も間違っていない位置__を指すエラーになる。
コミット点を置くとこれが直る:

> "1 + "  →  pos 4: expecting '(', '-', <number>   (包むと pos 2: expecting <end of input>)

この文法では成否も AST も変わらない。演算子の後には必ず項が来るので、
後戻りして成功する道が元々無いため (長さ5までの総当たりで確認済み)。

== まだ直っていないこと

@opp@ 自体が失敗したときは @Nothing@ が期待値集合を捨てるので、
@"1 2"@ は @expecting \<end of input\>@ になる。本来は演算子も並ぶべき。
位置は正しいので、これは hints (成功しても期待値だけ持ち越す) の課題。
-}
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p infixlp = p >>= restp p infixlp
 where
  -- lp と opp を引数で受け取るのは、外側の a を捕捉しないため。
  -- 捕捉すると restp は多相でなくなり、暗黙の forall b. が嘘になる。
  restp :: Parser b -> Parser (b -> b -> b) -> b -> Parser b
  restp lp opp lhs =
    optional opp >>= \case
      Nothing -> pure lhs
      Just op -> do
        rhs <- lp
        restp lp opp (op lhs rhs)

number :: Parser Expr
number = lexeme (Lit <$> (toDigit <$> digit >>= go))
 where
  toDigit b = fromIntegral (b - _0)
  go :: Integer -> Parser Integer
  go acc =
    optional digit >>= \case
      Nothing -> pure acc
      Just next -> go (10 * acc + toDigit next)

factor :: Parser Expr
factor =
  number
    <|> (symbol LParen *> expr <* symbol RParen)
    <|> (Neg <$> (symbol Minus *> factor))

term :: Parser Expr
term =
  chainl1
    factor
    ( symbol Star $> Bin Mul
        <|> symbol Slash $> Bin Div
    )

expr :: Parser Expr
expr =
  chainl1
    term
    ( symbol Plus $> Bin Add
        <|> symbol Minus $> Bin Sub
    )

{- | 入力全体を式として読む。文法を使う唯一の入口。

ここが2つの責任を果たす。

* __先頭の空白を落とす__。'lexeme' は空白をトークンの後ろでしか消費しないので、
  入力の先頭だけは誰の担当でもない。ここで一度だけ @many space@ する。
* __'eof' を要求する__。付けないと @\"1 2\"@ が @Lit 1@ として成功し、
  残りを黙って捨てる。部分一致を成功と呼ばないための歯止め。

'Cursor' の初期値を作るのもここだけ。他の関数は受け取った位置から進むだけで、
0 から始まることを知らない。
-}
parseExpr :: ByteString -> Either ParseError Expr
parseExpr src = fst <$> runParser (many space *> expr <* eof) src (Cursor 0)

-- | 評価が失敗する理由。今はゼロ除算だけだが、増えたら構成子を足す。
data EvalError = DivByZero
  deriving (Eq, Show)

{- | 構文木を評価する。ゼロ除算は 'Left' で返す。

例外ではなく 'Either' にしたのは、__失敗が型に現れる__ようにするため。
@Integer@ を返す関数は「必ず値が出る」と読めてしまい、呼び出し側が
ゼロ除算を考えなくてよいと誤解する余地がある。

除算は 'quot' (0 方向に切り捨て)。'div' は負の無限大方向に丸めるので
@(-7) \`div\` 2 == -4@、@(-7) \`quot\` 2 == -3@ と結果が変わる。
多くの言語の @\/@ は後者なので合わせた。整数除算しか無い文法なので、
どちらを選んだかは haddock でしか分からない。ここが唯一の記録。

'Bin' の左右は 'Either' の @do@ で順に評価するので、__左側の失敗が優先__される。
@1\/0 + 2\/0@ はどちらも 'DivByZero' なので差は見えないが、
理由を持つエラーが増えたときに効いてくる。
-}
eval :: Expr -> Either EvalError Integer
eval e = case e of
  Lit n -> pure n
  Neg x -> negate <$> eval x
  Bin op lhs rhs -> do
    a <- eval lhs
    b <- eval rhs
    case op of
      Add -> pure (a + b)
      Sub -> pure (a - b)
      Mul -> pure (a * b)
      Div -> if b == 0 then Left DivByZero else pure (a `quot` b)

-- テスト ----------------------------------------------------------------

{- | 構文木を完全に括弧付きの式に書き戻す。'reparse' の不変条件のためだけに使う。

全ての 'Bin' と 'Neg' を括弧で包むので、優先順位も結合も文字列の形で確定する。
つまり読み直した木は__元の木と一致するはず__で、一致しなければ
パーサーの優先順位か結合が木の形とずれている。

@Lit@ が非負である前提で書いている。負数は 'Neg' として木に現れるので、
パーサーが作る木に限れば成り立つ。
-}
render :: Expr -> ByteString
render e = case e of
  Lit n -> BC.pack (show n)
  Neg x -> "(-" <> render x <> ")"
  Bin op lhs rhs -> "(" <> render lhs <> opText op <> render rhs <> ")"
 where
  opText op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"

{- | 読んで、書き戻して、もう一度読む。結果は最初に読んだ木と等しいはず。

表と違い__入力を足せばタダで効く__不変条件 (docs/adr/0002)。
パースに失敗する入力でも 'Left' がそのまま伝播するので、
エラーの表にも同じようにかけられる。
-}
reparse :: ByteString -> Either ParseError Expr
reparse src = parseExpr src >>= parseExpr . render

{- | 'Label' の表示順が宣言順になること。

'Ord' を導出しているので構成子の宣言順が 'Set.toList' の昇順になり、
それがそのまま expecting の並びになる。構成子を並べ替えると表示順が変わる、
という繋がりは型にもコードにも書けないので、ここで固定する。
-}
labelOrder :: [Label] -> [String]
labelOrder = map renderLabel . Set.toList . Set.fromList

-- | パースが成功する入力と、その構文木。
parseCases :: [(ByteString, Either ParseError Expr)]
parseCases =
  [ ("1", ok (Lit 1))
  , ("42", ok (Lit 42))
  , ("007", ok (Lit 7))
  , ("   42   ", ok (Lit 42))
  , ("1+2", ok (Bin Add (Lit 1) (Lit 2)))
  , -- 左結合。右結合なら Bin Sub (Lit 1) (Bin Sub (Lit 2) (Lit 3))
    ("1-2-3", ok (Bin Sub (Bin Sub (Lit 1) (Lit 2)) (Lit 3)))
  , ("8/4/2", ok (Bin Div (Bin Div (Lit 8) (Lit 4)) (Lit 2)))
  , -- 優先順位。* が + より強い
    ("1+2*3", ok (Bin Add (Lit 1) (Bin Mul (Lit 2) (Lit 3))))
  , ("(1+2)*3", ok (Bin Mul (Bin Add (Lit 1) (Lit 2)) (Lit 3)))
  , ("2*(3+4)-5", ok (Bin Sub (Bin Mul (Lit 2) (Bin Add (Lit 3) (Lit 4))) (Lit 5)))
  , -- 単項マイナスは factor にあるので最も強く結合する
    ("-1*2", ok (Bin Mul (Neg (Lit 1)) (Lit 2)))
  , -- factor に再帰するので多重否定も通る
    ("--3", ok (Neg (Neg (Lit 3))))
  , (" 1 - -2 ", ok (Bin Sub (Lit 1) (Neg (Lit 2))))
  ]
 where
  ok = Right

-- | パースが失敗する入力と、その位置・期待集合。
errorCases :: [(ByteString, Either ParseError Expr)]
errorCases =
  [ ("", errAt 0 [Tok LParen, Tok Minus, Number])
  , -- chainl1 のコミット点。演算子を消費した先の pos 4 を指す
    ("1 + ", errAt 4 [Tok LParen, Tok Minus, Number])
  , ("1 + *", errAt 4 [Tok LParen, Tok Minus, Number])
  , ("(1+2", errAt 4 [Tok RParen])
  , -- eof を要求しているので部分一致では成功しない。
    -- 理想は演算子も並ぶこと (expecting '+' '-' '*' '/' <end of input>) だが、
    -- optional が期待値を捨てるので今は <end of input> だけ。hints の課題 (#14)
    ("1 2", errAt 2 [EndOfInput])
  , ("1)", errAt 1 [EndOfInput])
  ]
 where
  errAt p ls = Left (Unexpected p (Set.fromList ls))

{- | 入口から評価まで。

'ParseError' と 'EvalError' を束ねる型を作っていないので入れ子になる。
束ねなかった判断がそのまま型に出ている箇所。
-}
evalOf :: ByteString -> Either ParseError (Either EvalError Integer)
evalOf = fmap eval . parseExpr

-- | 評価の期待値。
evalCases :: [(ByteString, Either ParseError (Either EvalError Integer))]
evalCases =
  [ ("1+2", val 3)
  , ("1-2-3", val (-4))
  , ("8/4/2", val 1)
  , ("1+2*3", val 7)
  , ("(1+2)*3", val 9)
  , ("2*(3+4)-5", val 9)
  , ("--3", val 3)
  , ("7/2", val 3)
  , -- quot (0方向) を選んだことを固定する。div なら -4 になる
    ("-7/2", val (-3))
  , ("(0-7)/2", val (-3))
  , ("7/(0-2)", val (-3))
  , ("1/0", Right (Left DivByZero))
  , ("1/(2-2)", Right (Left DivByZero))
  , ("1+1/0", Right (Left DivByZero))
  , ("(1/0)*0", Right (Left DivByZero))
  ]
 where
  val = Right . Right

main :: IO ()
main =
  runTests
    [ runCases "parseExpr" parseExpr parseCases
    , runCases "parseExpr (失敗)" parseExpr errorCases
    , runCases "evalOf" evalOf evalCases
    , runCases "reparse (不変条件)" reparse [(src, parseExpr src) | (src, _) <- parseCases <> errorCases]
    , runCases
        "labelOrder"
        labelOrder
        [
          ( [Named "expr", EndOfInput, Number, Tok Slash, Tok Star, Tok Plus, Tok Minus, Tok RParen, Tok LParen]
          , ["'('", "')'", "'-'", "'+'", "'*'", "'/'", "<number>", "<end of input>", "<expr>"]
          )
        ]
    ]
