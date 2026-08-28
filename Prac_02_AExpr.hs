{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Prac_02_AExpr where

import Control.Applicative (Alternative (empty, (<|>)))
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

data TokenLabel = LParen | RParen | Minus | Plus | Star | Slash | Number | Space -- only labels with predicates
  deriving (Eq, Show, Ord)
data Label = Tok TokenLabel | EndOfInput | Named String -- what errors carry; free to grow
  deriving (Eq, Show, Ord)

renderLabel :: Label -> String
renderLabel l = case l of
  Tok LParen -> sym "("
  Tok RParen -> sym ")"
  Tok Minus -> sym "-"
  Tok Plus -> sym "+"
  Tok Star -> sym "*"
  Tok Slash -> sym "/"
  Tok Space -> term "space"
  Tok Number -> term "number"
  EndOfInput -> term "end of input"
  Named s -> sym s
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

satisfy :: TokenLabel -> Parser Word8
satisfy label = P \src cur ->
  case label of
    LParen -> go (== _parenleft) src cur
    RParen -> go (== _parenright) src cur
    Minus -> go (== _hyphen) src cur
    Plus -> go (== _plus) src cur
    Star -> go (== _asterisk) src cur
    Slash -> go (== _slash) src cur
    Number -> go isDigit src cur
    Space -> go isSpace src cur
 where
  go cond src cur = case src BS.!? pos cur of
    Nothing -> Left $ Unexpected (pos cur) (Set.singleton (Tok label))
    Just b ->
      if cond b
        then Right $ (b, cur{pos = pos cur + 1})
        else Left $ Unexpected (pos cur) (Set.singleton (Tok label))
eof :: Parser ()
eof = P \src cur -> case src BS.!? pos cur of
  Nothing -> pure ((), cur)
  Just _ -> Left $ Unexpected (pos cur) (Set.singleton EndOfInput)

