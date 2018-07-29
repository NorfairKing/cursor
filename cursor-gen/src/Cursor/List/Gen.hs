{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.List.Gen
    ( listCursorWithGen
    , listCursorWithIndex0
    ) where

import Test.QuickCheck

import Cursor.List

import Data.GenValidity

instance GenUnchecked a => GenUnchecked (ListCursor a)

instance GenValid a => GenValid (ListCursor a) where
    genValid = ListCursor <$> genValid <*> genValid

listCursorWithGen :: Gen a -> Gen (ListCursor a)
listCursorWithGen gen = ListCursor <$> genListOf gen <*> genListOf gen

listCursorWithIndex0 :: Gen a -> Gen (ListCursor a)
listCursorWithIndex0 gen = ListCursor [] <$> genListOf gen
