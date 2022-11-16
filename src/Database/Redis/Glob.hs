{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Database.Redis.Glob
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provide a type that models redis glob patterns
-}
module Database.Redis.Glob (
  -- * the Part datatype
  Part (..),
  InSquare (..),
  parseParts,
  parsePart,
  fromParts,
  fromPart,
  validate,
) where

import qualified ASCII.Char as A
import qualified ASCII.Superset as A
import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (($>))
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec
import qualified Text.Megaparsec.Byte as P


-- | Parse type for parsing 'ByteString' into Part
type Parser s m = (MonadParsec Void s m, Token s ~ Word8)


{- | Confirm that the glob pattern is valid

the result is:
- @Nothing@ when pattern is invalid
- Just @norm@, the the pattern is valid where norm is a normalized version of the input
-}
validate :: ByteString -> Maybe ByteString
validate = fmap fromParts . parseParts


parseInSquare :: (Parser s m, Token s ~ Word8) => m a -> m a
parseInSquare = between (P.char leftSquare) (P.char rightSquare)


notLeftSquare :: Parser s m => m Word8
notLeftSquare = satisfy (\x -> not $ hasRole x && x < 128)


notRightSquare :: Parser s m => m Word8
notRightSquare = satisfy (\x -> x /= rightSquare && x < 128)


escapedChar :: Parser s m => m Word8
escapedChar = P.char backslash *> P.asciiChar


matchable :: Parser s m => m Word8
matchable = try escapedChar <|> notLeftSquare


parseInRange :: Parser s m => m InSquare
parseInRange = do
  start <- notRightSquare
  _dash <- P.char dash
  InRange start <$> P.asciiChar


parseAnyInSquare :: Parser s m => m InSquare
parseAnyInSquare =
  choice
    [ try $ Single <$> escapedChar
    , try parseInRange
    , Single <$> notRightSquare
    ]


parseSquared :: Parser s m => m Part
parseSquared = parseInSquare $ do
  isNegated <- optional $ P.char hat
  x <- parseAnyInSquare
  xs <- many parseAnyInSquare
  pure $ maybe (Squared x xs) (const $ Negated x xs) isNegated


parseUnescaped :: Parser s m => m Part
parseUnescaped = do
  choice1 <- matchable
  others <- many matchable
  pure $ Unescaped choice1 others


parseAnyPart :: Parser s m => m Part
parseAnyPart =
  choice
    [ P.char star $> Many
    , P.char qmark $> Any
    , parseSquared
    , parseUnescaped
    ]


parseParts :: ByteString -> Maybe [Part]
parseParts = parseMaybe $ many parseAnyPart


parsePart :: ByteString -> Maybe Part
parsePart = parseMaybe parseAnyPart


fromPart :: Part -> ByteString
fromPart = toLazyByteString . builderOf


fromParts :: [Part] -> ByteString
fromParts = toLazyByteString . mconcat . map builderOf


class BuilderOf a where
  builderOf :: a -> Builder


instance BuilderOf InSquare where
  builderOf (Single x) = escaped8 x
  builderOf (InRange x y) = word8 x <> word8 dash <> word8 y


instance BuilderOf Part where
  builderOf Any = word8 qmark
  builderOf Many = word8 star
  builderOf (Unescaped x xs) = escaped8 x <> mconcat (map escaped8 xs)
  builderOf (Squared x xs) = inSquare $ builderOf x <> mconcat (map builderOf xs)
  builderOf (Negated x xs) = inSquare $ word8 hat <> builderOf x <> mconcat (map builderOf xs)


-- | Represents part of a valid redis glob pattern.
data Part
  = Any
  | Many
  | Unescaped Word8 [Word8]
  | Squared InSquare [InSquare]
  | Negated InSquare [InSquare]
  deriving (Eq, Show)


data InSquare
  = Single Word8
  | InRange Word8 Word8
  deriving (Eq, Show)


escaped8 :: Word8 -> Builder
escaped8 x | hasRole x = escaped x
escaped8 x = word8 x


escaped :: Word8 -> Builder
escaped = (word8 backslash <>) . word8


hasRole :: Word8 -> Bool
hasRole x = x == star || x == backslash || x == leftSquare || x == qmark


inSquare :: Builder -> Builder
inSquare inside = word8 leftSquare <> inside <> word8 rightSquare


hat, qmark, star, leftSquare, rightSquare, dash, backslash :: Word8
hat = A.fromChar A.Caret
qmark = A.fromChar A.QuestionMark
star = A.fromChar A.Asterisk
leftSquare = A.fromChar A.LeftSquareBracket
rightSquare = A.fromChar A.RightSquareBracket
dash = A.fromChar A.HyphenMinus
backslash = A.fromChar A.Backslash
