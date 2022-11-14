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
  parsePart,
  parseParts,
  fromPart,
  fromParts,
  validate,
) where

import qualified ASCII.Char as A
import qualified ASCII.Superset as A
import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (($>), (<&>))
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec
import qualified Text.Megaparsec.Byte as P


-- | Parse type for parsing 'ByteString' into Parts
type Parser s m = (MonadParsec Void s m, Token s ~ Word8)


{- | Confirm that the glob pattern is valid

the result is:
- @Nothing@ when pattern is invalid
- Just @norm@, the the pattern is valid where norm is a normalized version of the input
-}
validate :: ByteString -> Maybe ByteString
validate = fmap fromParts . parseParts


parseParts :: ByteString -> Maybe [Part]
parseParts = parseMaybe $ many parseAnyPart


parsePart :: ByteString -> Maybe Part
parsePart = parseMaybe parseAnyPart


parseAnyPart :: Parser s m => m Part
parseAnyPart =
  choice
    [ parseMany
    , parseAny
    , try parseRange
    , try parseExcept
    , try parseChoice
    , try parseSquareSingle
    , parseUnescaped
    ]


parseAny :: Parser s m => m Part
parseAny = P.char qmark $> Any


parseSquareSingle :: Parser s m => m Part
parseSquareSingle = parseInSquare $ notRightSquare <&> flip Unescaped []


parseMany :: Parser s m => m Part
parseMany = P.char star $> Many


parseRange :: Parser s m => m Part
parseRange = parseInSquare $ do
  start <- notRightSquare
  _dash <- P.char dash
  end <- P.asciiChar
  pure $ Range start end


parseChoice :: Parser s m => m Part
parseChoice = parseInSquare $ do
  choice1 <- notRightSquare
  choice2 <- notRightSquare
  others <- many notRightSquare
  pure $ Choice choice1 choice2 others


parseExcept :: Parser s m => m Part
parseExcept = parseInSquare $ do
  _hat <- P.char hat
  choice1 <- notRightSquare
  others <- many notRightSquare
  pure $ Except choice1 others


parseUnescaped :: Parser s m => m Part
parseUnescaped = do
  choice1 <- matchable
  others <- many matchable
  pure $ Unescaped choice1 others


parseInSquare :: (Parser s m, Token s ~ Word8) => m a -> m a
parseInSquare = between (P.char leftSquare) (P.char rightSquare)


notLeftSquare :: Parser s m => m Word8
notLeftSquare = satisfy (\x -> not $ hasPurpose x && x < 128)


notRightSquare :: Parser s m => m Word8
notRightSquare = satisfy (\x -> x /= rightSquare && x < 128)


escapedChar :: Parser s m => m Word8
escapedChar = P.char backslash *> P.asciiChar


matchable :: Parser s m => m Word8
matchable = try escapedChar <|> notLeftSquare


-- | Represents part of a valid redis glob pattern.
data Part
  = -- | == '?'
    Any
  | -- | == '*'
    Many
  | -- | e.g, '[a-c]'
    Range Word8 Word8
  | -- | e.g, '[ae]'
    Choice Word8 Word8 [Word8]
  | -- | e.g, '[^ae]'
    Except Word8 [Word8]
  | -- | e.g, 'abc'
    Unescaped Word8 [Word8]
  deriving (Show, Eq)


fromPart :: Part -> ByteString
fromPart = toLazyByteString . asBuilder


fromParts :: [Part] -> ByteString
fromParts = toLazyByteString . mconcat . map asBuilder


asBuilder :: Part -> Builder
asBuilder Any = word8 qmark
asBuilder Many = word8 star
asBuilder (Range from to) = inSquare $ word8 from <> word8 dash <> word8 to
asBuilder (Choice x x' xs) = inSquare $ word8 x <> word8 x' <> several xs
asBuilder (Except x xs) = inSquare $ word8 hat <> word8 x <> several xs
asBuilder (Unescaped x xs) = escaped8 x <> mconcat (map escaped8 xs)


several :: [Word8] -> Builder
several = mconcat . map word8


escaped8 :: Word8 -> Builder
escaped8 x | hasPurpose x = escaped x
escaped8 x = word8 x


escaped :: Word8 -> Builder
escaped = (word8 backslash <>) . word8


hasPurpose :: Word8 -> Bool
hasPurpose x = x == star || x == backslash || x == leftSquare || x == qmark


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
