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

import Cursor.Simple.List.NonEmpty
import Cursor.Simple.List.NonEmpty.Gen

spec :: Spec
spec = do
  eqSpec @(NonEmptyCursor Bool)
  genValidSpec @(NonEmptyCursor Bool)
  describe "makeNonEmptyCursor" $
    it "produces valid cursors" $ producesValidsOnValids (makeNonEmptyCursor @Bool)
  describe "makeNonEmptyCursorWithSelection" $ do
    it "produces valid cursors" $ producesValidsOnValids2 (makeNonEmptyCursorWithSelection @Bool)
    it "is the inverse of rebuildNonEmptyCursor when using the current selection" $
      forAllValid $ \lec ->
        makeNonEmptyCursorWithSelection
          (nonEmptyCursorSelection @Bool lec)
          (rebuildNonEmptyCursor lec) `shouldBe`
        Just lec
  describe "singletonNonEmptyCursor" $
    it "produces valid cursors" $ producesValidsOnValids (singletonNonEmptyCursor @Bool @Bool)
  describe "rebuildNonEmptyCursor" $ do
    it "produces valid nonempty lists" $ producesValidsOnValids (rebuildNonEmptyCursor @Bool)
    it "is the inverse of makeNonEmptyCursor for integers" $
      inverseFunctions (makeNonEmptyCursor @Int) rebuildNonEmptyCursor
    it "is the inverse of makeNonEmptyCursorWithSelection for integers, for any index" $
      forAll genUnchecked $ \i ->
        inverseFunctionsIfFirstSucceedsOnValid
          (makeNonEmptyCursorWithSelection @Int i)
          rebuildNonEmptyCursor
  describe "nonEmptyCursorElemL" $ lensSpecOnValid (nonEmptyCursorElemL @Bool @Bool)
  describe "nonEmptyCursorSelectPrev" $ do
    it "produces valid cursors" $ producesValidsOnValids (nonEmptyCursorSelectPrev @Bool)
    it "is a movement" $ isMovementM nonEmptyCursorSelectPrev
    it "selects the previous element" pending
  describe "nonEmptyCursorSelectNext" $ do
    it "produces valid cursors" $ producesValidsOnValids (nonEmptyCursorSelectNext @Bool)
    it "is a movement" $ isMovementM nonEmptyCursorSelectNext
    it "selects the next element" pending
  describe "nonEmptyCursorSelectFirst" $ do
    it "produces valid cursors" $ producesValidsOnValids (nonEmptyCursorSelectFirst @Bool)
    it "is a movement" $ isMovement nonEmptyCursorSelectFirst
    it "is idempotent" $ idempotentOnValid (nonEmptyCursorSelectFirst @Bool)
    it "selects the first element" pending
  describe "nonEmptyCursorSelectLast" $ do
    it "produces valid cursors" $ producesValidsOnValids (nonEmptyCursorSelectLast @Bool)
    it "is a movement" $ isMovement nonEmptyCursorSelectLast
    it "is idempotent" $ idempotentOnValid (nonEmptyCursorSelectLast @Bool)
    it "selects the last element" pending
  describe "nonEmptyCursorSelection" $ do
    it "produces valid ints" $ producesValidsOnValids (nonEmptyCursorSelection @Bool @Bool)
    it "returns the index of the currently selected element" pending
  describe "nonEmptyCursorSelectIndex" $ do
    it "produces valid cursors" $ producesValidsOnValids2 (nonEmptyCursorSelectIndex @Bool)
    it "is the identity function when given the current selection" $
      forAllValid $ \nec ->
        nonEmptyCursorSelectIndex (nonEmptyCursorSelection nec) nec `shouldBe`
        Just (nec :: NonEmptyCursor Bool)
    it "returns selects the element at the given index" pending
  describe "nonEmptyCursorInsert" $ do
    it "produces valid cursors" $
      forAllValid $ \d -> producesValidsOnValids (nonEmptyCursorInsert @Bool @Bool d)
    it "inserts a character before the cursor" pending
  describe "nonEmptyCursorAppend" $ do
    it "produces valid cursors" $
      forAllValid $ \d -> producesValidsOnValids (nonEmptyCursorAppend @Bool @Bool d)
    it "inserts a character after the cursor" pending
  describe "nonEmptyCursorInsertAndSelect" $ do
    it "produces valid cursors" $
      forAllValid $ \d -> producesValidsOnValids (nonEmptyCursorInsertAndSelect @Bool d)
    it "inserts a character before the cursor and selects it" pending
  describe "nonEmptyCursorAppendAndSelect" $ do
    it "produces valid cursors" $
      forAllValid $ \d -> producesValidsOnValids (nonEmptyCursorAppendAndSelect @Bool d)
    it "appends a character before the cursor and selects it" pending
  describe "nonEmptyCursorInsertAtStart" $ do
    it "produces valid cursors" $
      forAllValid $ \d -> producesValidsOnValids (nonEmptyCursorInsertAtStart @Bool @Bool d)
    it "inserts a character at the start of the list" pending
  describe "nonEmptyCursorAppendAtEnd" $ do
    it "produces valid cursors" $
      forAllValid $ \d -> producesValidsOnValids (nonEmptyCursorAppendAtEnd @Bool @Bool d)
    it "inserts a character at the end of the list" pending
  describe "nonEmptyCursorInsertAtStartAndSelect" $ do
    it "produces valid cursors" $
      forAllValid $ \d -> producesValidsOnValids (nonEmptyCursorInsertAtStartAndSelect @Bool d)
    it "inserts a character at the start of the list and selects it" pending
  describe "nonEmptyCursorAppendAtEndAndSelect" $ do
    it "produces valid cursors" $
      forAllValid $ \d -> producesValidsOnValids (nonEmptyCursorAppendAtEndAndSelect @Bool d)
    it "appends a character at the end of the list and selects it" pending
  describe "nonEmptyCursorRemoveElem" $ do
    it "produces valid cursors" $ producesValidsOnValids (nonEmptyCursorRemoveElem @Bool)
    it "removes an element" pending
  describe "nonEmptyCursorDeleteElem" $
    it "produces valid cursors" $ producesValidsOnValids (nonEmptyCursorDeleteElem @Bool)
  describe "nonEmptyCursorSearch" $ do
    it "produces valid cursors when looking for an equal element" $
      forAllValid $ \a -> producesValidsOnValids $ nonEmptyCursorSearch (== (a :: Bool))
    it "is indeed the right value when it finds a value and is looking for an equal element" $
      forAllValid $ \a ->
        forAllValid $ \nec ->
          case nonEmptyCursorSearch (== (a :: Bool)) nec of
            Nothing -> pure ()
            Just e -> nonEmptyCursorCurrent e `shouldBe` a
    it "finds an element if it is in there" $
      forAllValid $ \a ->
        forAll (nonEmptyWith a genValid) $ \nec ->
          case nonEmptyCursorSearch (== (a :: Bool)) nec of
            Nothing -> expectationFailure "Should not have failed to find the element."
            Just e -> nonEmptyCursorCurrent e `shouldBe` a
  describe "nonEmptyCursorSelectOrAdd" $
    it "produces valid cursors when looking for an equal element" $
    forAllValid $ \a -> producesValidsOnValids $ nonEmptyCursorSelectOrAdd (== a) (a :: Bool)

isMovementM :: (forall a. NonEmptyCursor a -> Maybe (NonEmptyCursor a)) -> Property
isMovementM func =
  forAllValid $ \lec ->
    case func (lec :: NonEmptyCursor Bool) of
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
    rebuildNonEmptyCursor (lec :: NonEmptyCursor Bool) `shouldBe` rebuildNonEmptyCursor (func lec)
