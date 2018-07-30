{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.ForestSpec
    ( spec
    ) where

import Test.Hspec

import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

import Control.Monad (unless)

import Cursor.Forest
import Cursor.Forest.Gen ()

spec :: Spec
spec = do
    eqSpec @(ForestCursor Int)
    functorSpec @ForestCursor
    genValidSpec @(ForestCursor Double)
    describe "makeForestCursor" $
        it "produces valid cursors" $
        producesValidsOnValids (makeForestCursor @Double)
    describe "rebuildForestCursor" $ do
        it "produces valid forests" $
            producesValidsOnValids (rebuildForestCursor @Double)
        it "is the inverse of makeForestCursor for integers" $
            inverseFunctions (makeForestCursor @Int) rebuildForestCursor
    describe "forestCursorListCursorL" $
        lensSpecOnValid (forestCursorListCursorL @Double)
    describe "forestCursorSelectedTreeL" $
        lensSpecOnValid (forestCursorSelectedTreeL @Double)
    describe "forestCursorSelectPrevTree" $ do
        testMovementM forestCursorSelectPrevTree
        it "selects the previous tree" pending
    describe "forestCursorSelectNextTree" $ do
        testMovementM forestCursorSelectNextTree
        it "selects the next tree" pending
    describe "forestCursorSelectFirstTree" $ do
        testMovement forestCursorSelectFirstTree
        it "selects the first tree" pending
    describe "forestCursorSelectLastTree" $ do
        testMovement forestCursorSelectLastTree
        it "selects the last tree" pending

testMovement :: (forall a. ForestCursor a -> ForestCursor a) -> Spec
testMovement func = do
    it "produces valids on valids" $ producesValidsOnValids $ func @Double
    it "is a movement" $ isMovement func

testMovementM :: (forall a. ForestCursor a -> Maybe (ForestCursor a)) -> Spec
testMovementM func = do
    it "produces valids on valids" $ producesValidsOnValids $ func @Double
    it "is a movement" $ isMovementM func

isMovementM :: (forall a. ForestCursor a -> Maybe (ForestCursor a)) -> Property
isMovementM func =
    forAllValid @(ForestCursor Int) $ \lec ->
        case func lec of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildForestCursor lec
                    ne' = rebuildForestCursor lec'
                in unless (ne == ne') $
                   expectationFailure $
                   unlines
                       [ "Cursor before:\n" ++ show lec
                       , "Forest before:  \n" ++ show ne
                       , "Cursor after: \n" ++ show lec'
                       , "Forest after:   \n" ++ show ne'
                       ]

isMovement :: (forall a. ForestCursor a -> ForestCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildForestCursor (lec :: ForestCursor Int) `shouldBe`
        rebuildForestCursor (func lec)
