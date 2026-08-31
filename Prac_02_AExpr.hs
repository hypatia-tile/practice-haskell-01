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

core :: Set Label -> (Word8 -> Bool) -> Parser Word8
core labels cond = P \src cur ->
  let basePos = pos cur
   in case src BS.!? basePos of
        Just b | cond b -> Right (b, cur{pos = basePos + 1})
        _ -> Left $ Unexpected basePos labels

satisfy :: TokenLabel -> Parser Word8
satisfy label =
  let toks = Set.singleton (Tok label)
   in case label of
        LParen -> core toks (== _parenleft)
        RParen -> core toks (== _parenright)
        Minus -> core toks (== _hyphen)
        Plus -> core toks (== _plus)
        Star -> core toks (== _asterisk)
        Slash -> core toks (== _slash)
        Number -> core toks isDigit
        Space -> core toks isSpace

space :: Parser Word8
space = core Set.empty isSpace

eof :: Parser ()
eof = P \src cur -> case src BS.!? pos cur of
  Nothing -> pure ((), cur)
  Just _ -> Left $ Unexpected (pos cur) (Set.singleton EndOfInput)
