{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.NonEmptySpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Control.Monad

import qualified Data.List.NonEmpty as NE

-- import Data.List.NonEmpty (NonEmpty)
import Cursor.NonEmpty
import Cursor.NonEmpty.Gen ()

spec :: Spec
spec = do
    eqSpec @(NonEmptyCursor Int)
    functorSpec @NonEmptyCursor
    genValidSpec @(NonEmptyCursor Double)
    describe "makeNonEmptyCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeNonEmptyCursor @Double)
    describe "makeNonEmptyCursorWithSelection" $
        it "produces valid cursors" $
        producesValidsOnValids2 (makeNonEmptyCursorWithSelection @Double)
    describe "singletonNonEmptyCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (singletonNonEmptyCursor @Double)
    describe "rebuildNonEmptyCursor" $ do
        it "produces valid nonempty lists" $
            producesValidsOnValids (rebuildNonEmptyCursor @Double)
        it "is the inverse of makeNonEmptyCursor for integers" $
            inverseFunctions (makeNonEmptyCursor @Int) rebuildNonEmptyCursor
        it
            "is the inverse of makeNonEmptyCursorWithSelection for integers, for any index" $
            forAll genUnchecked $ \i ->
                inverseFunctions
                    (makeNonEmptyCursorWithSelection @Int i)
                    rebuildNonEmptyCursor
    describe "nonEmptyCursorSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids (nonEmptyCursorSelectPrev @Double)
        it "is a movement" $ isMovementM nonEmptyCursorSelectPrev
        it "selects the previous element" pending
    describe "nonEmptyCursorSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids (nonEmptyCursorSelectNext @Double)
        it "is a movement" $ isMovementM nonEmptyCursorSelectNext
        it "selects the next element" pending
    describe "nonEmptyCursorSelectFirst" $ do
        it "produces valid cursors" $
            producesValidsOnValids (nonEmptyCursorSelectFirst @Double)
        it "is a movement" $ isMovement nonEmptyCursorSelectFirst
        it "is idempotent" $ idempotent (nonEmptyCursorSelectFirst @Double)
        it "selects the first element" pending
    describe "nonEmptyCursorSelectLast" $ do
        it "produces valid cursors" $
            producesValidsOnValids (nonEmptyCursorSelectLast @Double)
        it "is a movement" $ isMovement nonEmptyCursorSelectLast
        it "is idempotent" $ idempotent (nonEmptyCursorSelectLast @Double)
        it "selects the last element" pending
    describe "nonEmptyCursorInsert" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids (nonEmptyCursorInsert @Double d)
        it "inserts a character before the cursor" pending
    describe "nonEmptyCursorAppend" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids (nonEmptyCursorAppend @Double d)
        it "inserts a character after the cursor" pending
    describe "nonEmptyCursorInsertAndSelect" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids (nonEmptyCursorInsertAndSelect @Double d)
        it "inserts a character before the cursor and selects it" pending
    describe "nonEmptyCursorAppendAndSelect" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids (nonEmptyCursorAppendAndSelect @Double d)
        it "appends a character before the cursor and selects it" pending
    describe "nonEmptyCursorRemoveElem" $ do
        it "produces valid cursors" $
            producesValidsOnValids (nonEmptyCursorRemoveElem @Double)
        it "removes an element" pending
    describe "nonEmptyCursorDeleteElem" $ do
        it "produces valid cursors" $
            producesValidsOnValids (nonEmptyCursorDeleteElem @Double)
        it "deletes an element" pending
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

isMovementM ::
       (forall a. NonEmptyCursor a -> Maybe (NonEmptyCursor a)) -> Property
isMovementM func =
    forAllValid $ \lec ->
        case func (lec :: NonEmptyCursor Double) of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildNonEmptyCursor lec
                    ne' = rebuildNonEmptyCursor lec'
                 in unless (ne == ne') $
                    expectationFailure $
                    unlines
                        [ "Cursor before:\n" ++ show lec
                        , "List before:  \n" ++ show ne
                        , "Cursor after: \n" ++ show lec'
                        , "List after:   \n" ++ show ne'
                        ]

isMovement :: (forall a. NonEmptyCursor a -> NonEmptyCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildNonEmptyCursor (lec :: NonEmptyCursor Double) `shouldBe`
        rebuildNonEmptyCursor (func lec)
