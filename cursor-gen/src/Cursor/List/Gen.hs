{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.List.Gen
  ( listCursorWithGen,
    listCursorWithIndex0,
  )
where

import Cursor.List
import Data.GenValidity
import Test.QuickCheck

instance (GenValid a) => GenValid (ListCursor a) where
  genValid =
    sized $ \n -> do
      (a, b) <- genSplit n
      listCursorPrev <- resize a genValid
      listCursorNext <- resize b genValid
      pure ListCursor {..}
  shrinkValid (ListCursor prev next) =
    [ListCursor prev' next' | (prev', next') <- shrinkValid (prev, next)]

listCursorWithGen :: Gen a -> Gen (ListCursor a)
listCursorWithGen gen = ListCursor <$> genListOf gen <*> genListOf gen

listCursorWithIndex0 :: Gen a -> Gen (ListCursor a)
listCursorWithIndex0 gen = ListCursor [] <$> genListOf gen
