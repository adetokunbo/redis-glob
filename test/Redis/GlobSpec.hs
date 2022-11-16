{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Redis.GlobSpec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Redis.GlobSpec (spec) where

import qualified ASCII.Char as A
import qualified ASCII.Superset as A
import Data.Word (Word8)
import Database.Redis.Glob
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = describe "Glob" $ do
  context "parsePart" $ do
    it "should roundtrip with 'fromPart'" prop_trip

  context "parseParts" $ do
    it "should roundtrip with 'fromParts'" prop_multi_trip


prop_multi_trip :: Property
prop_multi_trip =
  withMaxSuccess 15000 $
    forAll (listOf genTwoParts) $ \ps -> do
      let allParts = concat ps
      parseParts (fromParts allParts) == Just allParts


prop_trip :: Property
prop_trip =
  withMaxSuccess 15000 $
    forAll genParts $ \p -> parsePart (fromPart p) == Just p


-- Avoid generating:
-- An InRange that starts with hat; this parses except when it's the first item
-- A single that just contains a dash; surrounded by other singles this mimicks InRange
-- An InRange with that starts with a dash, when preceded by a single this parses as the wrong InRange
-- A single 'hat', when it is first char, it changes a Squared to a Negated
genInSquare :: Gen InSquare
genInSquare =
  frequency
    [ (3, Single <$> (avoidHat . avoidDash) genInnerChar)
    , (1, InRange <$> (avoidHat . avoidDash) (avoid A.Backslash genInnerChar) <*> genInnerChar)
    ]


genParts :: Gen Part
genParts =
  frequency
    [ (1, pure Any)
    , (1, pure Many)
    , (2, Squared <$> genInSquare <*> listOf genInSquare)
    , (2, Negated <$> genInSquare <*> listOf genInSquare)
    , (3, Unescaped <$> genPrintable <*> listOf genPrintable)
    ]


-- when testing the parsing of list of parts, to ensure that no two succesive
-- parts are unescaped, the list of parts is generated in twos, where the first
-- part cannot be unescaped
genTwoParts :: Gen [Part]
genTwoParts = do
  part <- genParts
  special <- genSpecials
  pure [special, part]


genSpecials :: Gen Part
genSpecials =
  frequency
    [ (1, pure Any)
    , (1, pure Many)
    , (2, Squared <$> genInSquare <*> listOf genInSquare)
    , (2, Negated <$> genInSquare <*> listOf genInSquare)
    ]


genPrintable :: Gen Word8
genPrintable = chooseEnum (32, 127)


genInnerChar :: Gen Word8
genInnerChar = genPrintable `suchThat` (/= A.fromChar A.RightSquareBracket)


avoidHat :: Gen Word8 -> Gen Word8
avoidHat = avoid A.Caret


avoidDash :: Gen Word8 -> Gen Word8
avoidDash = avoid A.HyphenMinus


avoid :: A.Char -> Gen Word8 -> Gen Word8
avoid x = (`suchThat` (/= A.fromChar x))
