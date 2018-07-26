{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.List.Gen where

import Cursor.List

import Data.GenValidity

instance GenUnchecked a => GenUnchecked (ListCursor a)

instance GenValid a => GenValid (ListCursor a) where
    genValid  = ListCursor <$> genValid <*> genValid
