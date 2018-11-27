{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.CycleSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Data.Maybe

import Control.Monad

import Cursor.Cycle
import Cursor.Cycle.Gen ()

spec :: Spec
spec = do
    eqSpec @(CycleCursor Int)
    functorSpec @CycleCursor
    genValidSpec @(CycleCursor Rational)
    describe "emptyCycleCursor" $
        it "is valid" $ shouldBeValid (emptyCycleCursor @Int)
    describe "makeCycleCursor" $
        it "produces valid cycle cursors" $
        producesValidsOnValids (makeCycleCursor @Rational)
    describe "makeCycleCursorWithSelection" $
        it "produces valid cycle cursors" $
        producesValidsOnValids2 (makeCycleCursorWithSelection @Rational)
    describe "rebuildCycleCursor" $ do
        it "produces valid cycles" $
            producesValidsOnValids (rebuildCycleCursor @Rational)
        it "is the inverse of makeCycleCursor" $
            inverseFunctions (makeCycleCursor @Int) rebuildCycleCursor
        it "is the inverse of makeCycleCursorWithSelection for any index" $
            forAllUnchecked $ \i ->
                inverseFunctionsIfFirstSucceeds
                    (makeCycleCursorWithSelection @Int i)
                    rebuildCycleCursor
    describe "cycleCursorNull" $
        it "produces valid bools" $
        producesValidsOnValids (cycleCursorNull @Rational)
    describe "cycleCursorLength" $
        it "produces valid bools" $
        producesValidsOnValids (cycleCursorLength @Rational)
    describe "cycleCursorIndex" $
        it "produces valid indices" $
        producesValidsOnValids (cycleCursorIndex @Rational)
    describe "cycleCursorSelectPrev" $ do
        it "produces valid cursors" $
            producesValidsOnValids (cycleCursorSelectPrev @Rational)
        it "is a movement" $ isMovementM cycleCursorSelectPrev
        it "is the inverse of cycleCursorSelectNext" $
            inverseFunctionsIfSucceed
                (cycleCursorSelectNext @Rational)
                cycleCursorSelectPrev
        it "fails for an empty cycle cursor" $
            cycleCursorSelectPrev (emptyCycleCursor @Rational) `shouldBe`
            Nothing
        it "succeeds for a nonempty cycle cursor" $
            forAllValid $ \a ->
                forAllValid $ \as ->
                    cycleCursorSelectPrev
                        (makeCycleCursor ((a :: Rational) : as)) `shouldSatisfy`
                    isJust
        it "selects the previous position" pending
    describe "cycleCursorSelectNext" $ do
        it "produces valid cursors" $
            producesValidsOnValids (cycleCursorSelectNext @Rational)
        it "is a movement" $ isMovementM cycleCursorSelectNext
        it "is the inverse of cycleCursorSelectPrev" $
            inverseFunctionsIfSucceed
                (cycleCursorSelectPrev @Rational)
                cycleCursorSelectNext
        it "fails for an empty cycle cursor" $
            cycleCursorSelectNext (emptyCycleCursor @Rational) `shouldBe`
            Nothing
        it "succeeds for a nonempty cycle cursor" $
            forAllValid $ \a ->
                forAllValid $ \as ->
                    cycleCursorSelectNext
                        (makeCycleCursor ((a :: Rational) : as)) `shouldSatisfy`
                    isJust
        it "selects the next position" pending
    describe "cycleCursorSelectIndex" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 (cycleCursorSelectIndex @Rational)
        it "is a movement" $
            forAllUnchecked $ \ix -> isMovement (cycleCursorSelectIndex ix)
        it "selects the position at the given index" pending
    describe "cycleCursorPrevItem" $ do
        it "produces valid items" $
            producesValidsOnValids (cycleCursorPrevItem @Rational)
        it "fails for an empty cycle cursor" $
            cycleCursorPrevItem (emptyCycleCursor @Rational) `shouldBe` Nothing
        it "succeeds for a nonempty cycle cursor" $
            forAllValid $ \a ->
                forAllValid $ \as ->
                    cycleCursorPrevItem (makeCycleCursor ((a :: Rational) : as)) `shouldSatisfy`
                    isJust
        it "returns the item before the position" pending
    describe "cycleCursorNextItem" $ do
        it "produces valid items" $
            producesValidsOnValids (cycleCursorNextItem @Rational)
        it "fails for an empty cycle cursor" $
            cycleCursorNextItem (emptyCycleCursor @Rational) `shouldBe` Nothing
        it "succeeds for a nonempty cycle cursor" $
            forAllValid $ \a ->
                forAllValid $ \as ->
                    cycleCursorNextItem (makeCycleCursor ((a :: Rational) : as)) `shouldSatisfy`
                    isJust
        it "returns the item after the position" pending
    describe "cycleCursorSelectStart" $ do
        it "produces valid cursors" $
            producesValidsOnValids (cycleCursorSelectStart @Rational)
        it "is a movement" $ isMovement cycleCursorSelectStart
        it "is idempotent" $
            idempotentOnValid (cycleCursorSelectStart @Rational)
        it "selects the starting position" pending
    describe "cycleCursorSelectEnd" $ do
        it "produces valid cursors" $
            producesValidsOnValids (cycleCursorSelectEnd @Rational)
        it "is a movement" $ isMovement cycleCursorSelectEnd
        it "is idempotent" $ idempotentOnValid (cycleCursorSelectEnd @Rational)
        it "selects the end position" pending
    describe "cycleCursorInsert" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids (cycleCursorInsert @Rational d)
        it "inserts an item before the cursor" $ pending
    describe "cycleCursorAppend" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids (cycleCursorAppend @Rational d)
        it "inserts an item after the cursor" $ pending
    describe "cycleCursorRemove" $ do
        it "produces valids" $
            validIfSucceedsOnValid (cycleCursorRemove @Rational)
        it "removes an item before the cursor" $ pending
    describe "cycleCursorDelete" $ do
        it "produces valids" $
            validIfSucceedsOnValid (cycleCursorDelete @Rational)
        it "removes an item before the cursor" $ pending

isMovementM :: (forall a. CycleCursor a -> Maybe (CycleCursor a)) -> Property
isMovementM func =
    forAllValid $ \lec ->
        case func (lec :: CycleCursor Int) of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildCycleCursor lec
                    ne' = rebuildCycleCursor lec'
                 in unless (ne == ne') $
                    expectationFailure $
                    unlines
                        [ "Cursor before:\n" ++ show lec
                        , "Cycle before:  \n" ++ show ne
                        , "Cursor after: \n" ++ show lec'
                        , "Cycle after:   \n" ++ show ne'
                        ]

isMovement :: (forall a. CycleCursor a -> CycleCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildCycleCursor (lec :: CycleCursor Int) `shouldBe`
        rebuildCycleCursor (func lec)
