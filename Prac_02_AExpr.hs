{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Prac_02_AExpr where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Functor (($>))
import Data.Word8
import TestKit (runCases, runTests)

data Expr
  = Lit Integer
  | Neg Expr
  | Bin Op Expr Expr
  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

data Cursor = Cursor
  { source :: !ByteString
  , pos :: !Int
  }
  deriving (Show)

initCursor :: ByteString -> Cursor
initCursor s = Cursor s 0

data ParseError = ParseError
  { errPos :: !Int
  , errWant :: String
  }
  deriving (Eq, Show)

newtype Parser a = Parser {runParser :: Cursor -> Either ParseError (a, Cursor)}

throw :: String -> Parser a
throw msg = Parser \(Cursor _ pos) -> Left $ ParseError pos msg

modify :: (Cursor -> Cursor) -> Parser ()
modify f = Parser \s -> pure ((), f s)

getOffset :: Parser Int
getOffset = Parser \s -> pure (pos s, s)

get :: Parser Cursor
get = Parser \s -> pure (s, s)

gets :: (Cursor -> a) -> Parser a
gets f = Parser \s -> pure ((f s), s)

pGetSpan :: Int -> Cursor -> ByteString
pGetSpan anchor = \(Cursor s p) ->
  BS.take (p - anchor) . (BS.drop anchor) $ s

fstMap :: (a -> b) -> (a, c) -> (b, c)
fstMap f (x, y) = (f x, y)
{-# INLINE fstMap #-}

instance Functor Parser where
  fmap f (Parser p) = Parser \s ->
    fmap (fstMap f) $ p s

instance Applicative Parser where
  pure a = Parser \s -> pure $ (a, s)
  (Parser f) <*> (Parser p) = Parser \s ->
    case f s of
      Left e -> Left e
      Right (f', s') -> (fstMap f') <$> p s'

instance Monad Parser where
  (Parser p) >>= f = Parser \s -> do
    (a, c) <- p s
    let Parser q = f a
    q c

instance Alternative Parser where
  empty = Parser \_ -> Left $ ParseError 0 ""
  (Parser p) <|> (Parser q) = Parser \s ->
    case p s of
      Right v -> pure v
      Left _ -> q s

peek :: Parser (Maybe Word8)
peek = Parser \s -> pure (source s BS.!? pos s, s)

advance :: Parser (Word8)
advance = Parser \s@(Cursor source pos) -> case source BS.!? pos of
  Just b -> pure (b, s { pos = pos + 1})
  Nothing -> error "advance: program error"

satisfy :: (Word8 -> Bool) -> String -> Parser Word8
satisfy cond msg = do
  c <- peek
  case c of
    Just b -> if cond b then advance else throw msg
    Nothing -> throw "eof"

skip :: Parser ()
skip = Parser \s -> pure ((), s{pos = pos s + 1})

byte :: Word8 -> Parser Word8
byte = pure

spaces :: Parser ()
spaces = Parser \(Cursor s p) ->
  case BS.findIndex (not . isSpace) (BS.drop p s) of
    Just i -> pure $ ((), Cursor s (p + i + 1))
    Nothing -> pure $ ((), Cursor s (BS.length s))

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

pTakeWhile :: (Word8 -> Bool) -> Cursor -> Cursor
pTakeWhile cond (Cursor source offset) =
  case BS.findIndex cond (BS.drop offset source) of
    Just i -> Cursor source (offset + i)
    Nothing -> Cursor source (offset + BS.length source)

readNumber :: ByteString -> Integer
readNumber bits = case BC.readInteger bits of
  Nothing -> error "program error"
  Just i -> fst i

number :: Parser Integer
number = do
  offset <- getOffset
  modify (pTakeWhile isDigit)
  num <- gets $ pGetSpan offset
  if BS.null num
    then error "program error"
    else pure (readNumber num)

many :: Parser a -> Parser [a]
many p =
  ( do
      a <- p
      (a :) <$> many p
  )
    <|> pure []

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p q = do
  a <- p
  rests <- many (q >>= \i -> (\n -> (i, n)) <$> p)
  pure $ foldl' (\l (op, r) -> op l r) a rests

expr :: Parser Expr
expr = chainl1 term addOp

term :: Parser Expr
term = chainl1 factor mulOp

factor :: Parser Expr
factor = lit <|> paren <|> negate'

lit :: Parser Expr
lit = lexeme number >>= (pure . Lit)

paren :: Parser Expr
paren = lexeme do
  _ <- satisfy (_parenleft ==) "expect left paren"
  ex <- expr
  _ <- satisfy (_parenright ==) "expect right paren"
  pure ex

negate' :: Parser Expr
negate' = lexeme do
  _ <- satisfy (_mu ==) "expect minus sign"
  ex <- lit
  pure (Neg ex)

addOp :: Parser (Expr -> Expr -> Expr)
addOp =
  lexeme $
    (satisfy (_plus ==) "expect plus sign" $> Bin Add)
      <|> (satisfy (_mu ==) "expect minus sign" $> Bin Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp =
  lexeme $
    (satisfy (_asterisk ==) "expect asterisk" $> Bin Mul)
      <|> (satisfy (_slash ==) "expect slash" $> Bin Div)

parseExpr :: ByteString -> Either ParseError Expr
parseExpr = fmap fst . (runParser $ spaces *> expr) . initCursor

eval :: Expr -> Either String Integer
eval (Lit n) = Right n
eval (Neg ex) = negate <$> eval ex
eval (Bin op l r) = do
  lexp <- eval l
  rexp <- eval r
  case op of
    Add -> pure (lexp + rexp)
    Sub -> pure (lexp - rexp)
    Mul -> pure (lexp * rexp)
    Div -> if rexp == 0 then Left "zero division" else pure (lexp `div` rexp)

main :: IO ()
main =
  runTests
    [ runCases "parseExpr" parseExpr casesParse
    , runCases "eval" (fmap eval . parseExpr) caseEval
    ]

casesParse :: [(ByteString, Either ParseError Expr)]
casesParse =
  [ (("1+2"), Right (Bin Add (Lit 1) (Lit 2))) ]

caseEval :: [(ByteString, Either ParseError (Either String Integer))]
caseEval =
  [ (("1+2"), Right (Right 3)) ]
