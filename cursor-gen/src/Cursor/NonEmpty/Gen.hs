{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.NonEmpty.Gen
    ( nonEmptyElemOf
    , nonEmptyWithIndex0
    ) where

import Control.Monad

import Data.GenValidity

import Test.QuickCheck

import qualified Data.List.NonEmpty as NE

import Cursor.NonEmpty

instance GenUnchecked a => GenUnchecked (NonEmptyCursor a) where
    genUnchecked =
        sized $ \n -> do
            part <- arbPartition n
            case part of
                [] -> singletonNonEmptyCursor <$> resize 0 genUnchecked
                (s:ss) -> do
                    i <- choose (0, length ss)
                    let (as, bs) = splitAt i ss
                    nonEmptyCursorPrev <- forM as $ \s_ -> resize s_ genUnchecked
                    nonEmptyCursorCurrent <- resize s genUnchecked
                    nonEmptyCursorNext <- forM bs $ \s_ -> resize s_ genUnchecked
                    pure NonEmptyCursor {..}

instance GenValid a => GenValid (NonEmptyCursor a) where
    genValid =
        sized $ \n -> do
            part <- arbPartition n
            case part of
                [] -> singletonNonEmptyCursor <$> resize 0 genValid
                (s:ss) -> do
                    i <- choose (0, length ss)
                    let (as, bs) = splitAt i ss
                    nonEmptyCursorPrev <- forM as $ \s_ -> resize s_ genValid
                    nonEmptyCursorCurrent <- resize s genValid
                    nonEmptyCursorNext <- forM bs $ \s_ -> resize s_ genValid
                    pure NonEmptyCursor {..}

nonEmptyElemOf :: NonEmptyCursor a -> Gen a
nonEmptyElemOf = elements . NE.toList . rebuildNonEmptyCursor

nonEmptyWithIndex0 :: Gen a -> Gen (NonEmptyCursor a)
nonEmptyWithIndex0 g = NonEmptyCursor [] <$> g <*> genListOf g
