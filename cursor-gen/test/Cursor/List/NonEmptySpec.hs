{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.List.NonEmptySpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity

import qualified Data.List.NonEmpty as NE

import Cursor.List.NonEmpty

spec :: Spec
spec = do
    describe "nonemptyPrepend" $
        it "is equivalent to regular prepend" $
        equivalentWhenFirstSucceeds
            (\(ls1, ls2) ->
                 (NE.toList . nonemptyPrepend ls1) <$> NE.nonEmpty ls2)
            (uncurry (++) :: ([Int], [Int]) -> [Int])
    describe "nonemptyAppend" $
        it "is equivalent to regular append" $
        equivalentWhenFirstSucceeds
            (\(ls1, ls2) ->
                 (NE.toList . (`nonemptyAppend` ls2)) <$> NE.nonEmpty ls1)
            (uncurry (++) :: ([Int], [Int]) -> [Int])
