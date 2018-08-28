{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.List.Gen
    ( listCursorWithGen
    , listCursorWithIndex0
    ) where

import Test.QuickCheck

import Cursor.List

import Data.GenValidity

instance GenUnchecked a => GenUnchecked (ListCursor a) where
    genUnchecked =
        sized $ \n -> do
            (a, b) <- genSplit n
            listCursorPrev <- resize a genUnchecked
            listCursorNext <- resize b genUnchecked
            pure ListCursor {..}

instance GenValid a => GenValid (ListCursor a) where
    genValid =
        sized $ \n -> do
            (a, b) <- genSplit n
            listCursorPrev <- resize a genValid
            listCursorNext <- resize b genValid
            pure ListCursor {..}

listCursorWithGen :: Gen a -> Gen (ListCursor a)
listCursorWithGen gen = ListCursor <$> genListOf gen <*> genListOf gen

listCursorWithIndex0 :: Gen a -> Gen (ListCursor a)
listCursorWithIndex0 gen = ListCursor [] <$> genListOf gen
