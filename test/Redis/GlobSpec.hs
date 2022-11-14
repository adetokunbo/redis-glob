{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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


prop_trip :: Property
prop_trip =
  withMaxSuccess 15000 $
    forAll genPart $ \p -> parsePart (fromPart p) == Just p


prop_multi_trip :: Property
prop_multi_trip =
  withMaxSuccess 15000 $
    forAll (listOf genTwoParts) $ \ps -> do
      let allParts = concat ps
      parseParts (fromParts allParts) == Just allParts


genPart :: Gen Part
genPart =
  frequency
    [ (1, pure Any)
    , (1, pure Many)
    , (2, genRange)
    , (2, genChoice)
    , (2, genExcept)
    , (3, genUnescaped)
    ]


genSpecials :: Gen Part
genSpecials =
  frequency
    [ (1, pure Any)
    , (1, pure Many)
    , (2, genRange)
    , (2, genChoice)
    , (2, genExcept)
    ]


-- when testing the parsing of list of parts, to ensure that no two succesive
-- parts are unescaped, the list of parts is generated in twos, where the first
-- part cannot be unescaped
genTwoParts :: Gen [Part]
genTwoParts = do
  part <- genPart
  special <- genSpecials
  pure [special, part]


genRange :: Gen Part
genRange = Range <$> avoidHat genInnerChar <*> genPrintable


genChoice :: Gen Part
genChoice = Choice <$> avoidHat genInnerChar <*> avoidDash genInnerChar <*> listOf genInnerChar


genExcept :: Gen Part
genExcept = Except <$> avoidDash genInnerChar <*> listOf genInnerChar


genUnescaped :: Gen Part
genUnescaped = Unescaped <$> genPrintable <*> listOf genPrintable


genPrintable :: Gen Word8
genPrintable = chooseEnum (32, 127)


genInnerChar :: Gen Word8
genInnerChar = genPrintable `suchThat` (/= A.fromChar A.RightSquareBracket)


avoidHat :: Gen Word8 -> Gen Word8
avoidHat = (`suchThat` (/= A.fromChar A.Caret))


avoidDash :: Gen Word8 -> Gen Word8
avoidDash = (`suchThat` (/= A.fromChar A.HyphenMinus))
