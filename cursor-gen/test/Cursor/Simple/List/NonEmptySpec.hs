{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Simple.List.NonEmptySpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

import Control.Monad

import Cursor.List.NonEmpty.Gen
import Cursor.Simple.List.NonEmpty
import Cursor.Simple.List.NonEmpty.Gen ()

spec :: Spec
spec = do
    eqSpec @(NonEmptyCursor Int)
    genValidSpec @(NonEmptyCursor Double)
    describe "makeNonEmptyCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeNonEmptyCursor @Double)
    describe "makeNonEmptyCursorWithSelection" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (makeNonEmptyCursorWithSelection @Double)
        it
            "is the inverse of rebuildNonEmptyCursor when using the current selection" $
            forAllValid $ \lec ->
                makeNonEmptyCursorWithSelection
                    (nonEmptyCursorSelection @Double lec)
                    (rebuildNonEmptyCursor lec) `shouldBe`
                Just lec
    describe "singletonNonEmptyCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (singletonNonEmptyCursor @Double @Double)
    describe "rebuildNonEmptyCursor" $ do
        it "produces valid nonempty lists" $
            producesValidsOnValids (rebuildNonEmptyCursor @Double)
        it "is the inverse of makeNonEmptyCursor for integers" $
            inverseFunctions (makeNonEmptyCursor @Int) rebuildNonEmptyCursor
        it
            "is the inverse of makeNonEmptyCursorWithSelection for integers, for any index" $
            forAll genUnchecked $ \i ->
                inverseFunctionsIfFirstSucceedsOnValid
                    (makeNonEmptyCursorWithSelection @Int i)
                    rebuildNonEmptyCursor
    describe "nonEmptyCursorElemL" $
        lensSpecOnValid (nonEmptyCursorElemL @Double @Double)
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
        it "is idempotent" $
            idempotentOnValid (nonEmptyCursorSelectFirst @Double)
        it "selects the first element" pending
    describe "nonEmptyCursorSelectLast" $ do
        it "produces valid cursors" $
            producesValidsOnValids (nonEmptyCursorSelectLast @Double)
        it "is a movement" $ isMovement nonEmptyCursorSelectLast
        it "is idempotent" $
            idempotentOnValid (nonEmptyCursorSelectLast @Double)
        it "selects the last element" pending
    describe "nonEmptyCursorSelection" $ do
        it "produces valid ints" $
            producesValidsOnValids (nonEmptyCursorSelection @Double @Double)
        it "returns the index of the currently selected element" pending
    describe "nonEmptyCursorSelectIndex" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (nonEmptyCursorSelectIndex @Double)
        it "is the identity function when given the current selection" $
            forAllValid $ \nec ->
                nonEmptyCursorSelectIndex (nonEmptyCursorSelection nec) nec `shouldBe`
                Just (nec :: NonEmptyCursor Double)
        it "returns selects the element at the given index" pending
    describe "nonEmptyCursorInsert" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids (nonEmptyCursorInsert @Double @Double d)
        it "inserts a character before the cursor" pending
    describe "nonEmptyCursorAppend" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids (nonEmptyCursorAppend @Double @Double d)
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
    describe "nonEmptyCursorInsertAtStart" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (nonEmptyCursorInsertAtStart @Double @Double d)
        it "inserts a character at the start of the list" pending
    describe "nonEmptyCursorAppendAtEnd" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (nonEmptyCursorAppendAtEnd @Double @Double d)
        it "inserts a character at the end of the list" pending
    describe "nonEmptyCursorInsertAtStartAndSelect" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (nonEmptyCursorInsertAtStartAndSelect @Double d)
        it "inserts a character at the start of the list and selects it" pending
    describe "nonEmptyCursorAppendAtEndAndSelect" $ do
        it "produces valid cursors" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (nonEmptyCursorAppendAtEndAndSelect @Double d)
        it "appends a character at the end of the list and selects it" pending
    describe "nonEmptyCursorRemoveElem" $ do
        it "produces valid cursors" $
            producesValidsOnValids (nonEmptyCursorRemoveElem @Double)
        it "removes an element" pending
    describe "nonEmptyCursorDeleteElem" $ do
        it "produces valid cursors" $
            producesValidsOnValids (nonEmptyCursorDeleteElem @Double)
    describe "nonEmptyCursorSearch" $ do
        it "produces valid cursors when looking for an equal element" $
            forAllValid $ \a ->
                producesValidsOnValids $ nonEmptyCursorSearch (== (a :: Double))
        it
            "is indeed the right value when it finds a value and is looking for an equal element" $
            forAllValid $ \a ->
                forAllValid $ \nec ->
                    case nonEmptyCursorSearch (== (a :: Double)) nec of
                        Nothing -> pure ()
                        Just e -> nonEmptyCursorCurrent e `shouldBe` a
        it "finds an element if it is in there" $
            forAllValid $ \a ->
                forAll (nonEmptyWith a genValid) $ \nec ->
                    case nonEmptyCursorSearch (== (a :: Double)) nec of
                        Nothing ->
                            expectationFailure
                                "Should not have failed to find the element."
                        Just e -> nonEmptyCursorCurrent e `shouldBe` a
    describe "nonEmptyCursorSelectOrAdd" $ do
        it "produces valid cursors when looking for an equal element" $
            forAllValid $ \a ->
                producesValidsOnValids $ nonEmptyCursorSelectOrAdd (== a) (a :: Double)

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
