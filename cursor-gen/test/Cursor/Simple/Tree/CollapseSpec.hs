{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.Simple.Tree.CollapseSpec
  ( spec,
  )
where

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  describe "treeCursorOpenCurrentForest"
    $ it "produces valid cursors"
    $ producesValidsOnValids
    $ treeCursorOpenCurrentForest @Bool @Bool
  describe "treeCursorCloseCurrentForest"
    $ it "produces valid cursors"
    $ producesValidsOnValids
    $ treeCursorCloseCurrentForest @Bool @Bool
  describe "treeCursorToggleCurrentForest"
    $ it "produces valid cursors"
    $ producesValidsOnValids
    $ treeCursorToggleCurrentForest @Bool @Bool
  describe "treeCursorOpenCurrentForestRecursively"
    $ it "produces valid cursors"
    $ producesValidsOnValids
    $ treeCursorOpenCurrentForestRecursively @Bool @Bool
  describe "treeCursorToggleCurrentForestRecursively"
    $ it "produces valid cursors"
    $ producesValidsOnValids
    $ treeCursorToggleCurrentForestRecursively @Bool @Bool
