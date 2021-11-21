{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.Simple.Tree.InsertSpec
  ( spec,
  )
where

import Cursor.Simple.Tree hiding (TreeCursor)
import Cursor.Simple.Tree.Gen ()
import Cursor.Tree.Types
import Data.Tree
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  describe "treeCursorInsert" $ do
    it "produces valids on valids" $ producesValid2 $ treeCursorInsert @Bool @Bool
    it "inserts the element" pending
  describe "treeCursorInsertAndSelect" $ do
    it "produces valids on valids" $ producesValid2 $ treeCursorInsertAndSelect @Bool
    it "inserts and select the element" pending
  describe "treeCursorInsertNodeSingleAndSelect" $ do
    it "produces valids on valids" $ producesValid2 $ treeCursorInsertNodeSingleAndSelect @Bool
    it "inserts and select the element" pending
  describe "treeCursorInsertNodeAndSelect" $ do
    it "produces valids on valids" $ producesValid3 $ treeCursorInsertNodeAndSelect @Bool
    it "inserts and select the element" pending
  describe "treeCursorAppend" $ do
    it "produces valids on valids" $ producesValid2 $ treeCursorAppend @Bool @Bool
    it "appends the element" pending
  describe "treeCursorAppendAndSelect" $ do
    it "produces valids on valids" $ producesValid2 $ treeCursorAppendAndSelect @Bool
    it "appends and select the element" pending
  describe "treeCursorAppendNodeSingleAndSelect" $ do
    it "produces valids on valids" $ producesValid2 $ treeCursorAppendNodeSingleAndSelect @Bool
    it "appends and select the element" pending
  describe "treeCursorAppendNodeAndSelect" $ do
    it "produces valids on valids" $ producesValid3 $ treeCursorAppendNodeAndSelect @Bool
    it "appends and select the element" pending
  describe "treeCursorAddChildAtPos" $ do
    it "produces valid cursors " $ producesValid3 $ treeCursorAddChildAtPos @Bool @Bool
    it "adds a tree at the given index in the children of the current node" pending
  describe "treeCursorAddChildAtStart" $ do
    it "produces valid cursors " $ producesValid2 $ treeCursorAddChildAtStart @Bool @Bool
    it "adds a tree at the start of the children of the current node" pending
  describe "treeCursorAddChildAtEnd" $ do
    it "produces valid cursors " $ producesValid2 $ treeCursorAddChildAtEnd @Bool @Bool
    it "adds a tree at the end of the children of the current node" pending
  describe "treeCursorAddChildAtPosAndSelect" $ do
    it "produces valid cursors " $ producesValid3 $ treeCursorAddChildAtPosAndSelect @Bool
    it "adds a tree at the given index in theAndSelect children of the current node" pending
  describe "treeCursorAddChildAtStartAndSelect" $ do
    it "producesAndSelect valid cursors " $ producesValid2 $ treeCursorAddChildAtStartAndSelect @Bool
    it "adds a tree at the start of the children of the current node" pending
  describe "treeCursorAddChildAtEndAndSelect" $ do
    it "produces valid cursors " $ producesValid2 $ treeCursorAddChildAtEndAndSelect @Bool
    it "adds a tree at the end of the children of the current node" pending
  describe "treeCursorAddChildNodeSingleAtPosAndSelect" $ it "produces valid cursors" $ producesValid3 (treeCursorAddChildNodeSingleAtPosAndSelect @Bool)
  describe "treeCursorAddChildNodeSingleAtStartAndSelect" $ it "produces valid cursors" $ producesValid2 (treeCursorAddChildNodeSingleAtStartAndSelect @Bool)
  describe "treeCursorAddChildNodeSingleAtEndAndSelect" $ it "produces valid cursors" $ producesValid2 (treeCursorAddChildNodeSingleAtEndAndSelect @Bool)
  describe "treeCursorAddChildNodeAtPosAndSelect" $ it "produces valid cursors" $ forAllValid $ producesValid3 . (treeCursorAddChildNodeAtPosAndSelect @Bool)
  describe "treeCursorAddChildNodeAtStartAndSelect" $ do
    it "produces valid cursors" $ producesValid3 (treeCursorAddChildNodeAtStartAndSelect @Bool)
    it "works for this example" $
      let tc =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = map makeCTree [Node "1" [], Node "2" []],
                        treeAboveAbove = Nothing,
                        treeAboveNode = "bar",
                        treeAboveRights = map makeCTree [Node "3" [], Node "4" []]
                      },
                treeCurrent = "baz",
                treeBelow = makeCForest [Node "a" [], Node "b" []]
              }
          tc' =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = [],
                        treeAboveAbove =
                          Just
                            TreeAbove
                              { treeAboveLefts = map makeCTree [Node "1" [], Node "2" []],
                                treeAboveAbove = Nothing,
                                treeAboveNode = "bar",
                                treeAboveRights = map makeCTree [Node "3" [], Node "4" []]
                              },
                        treeAboveNode = "baz",
                        treeAboveRights = map makeCTree [Node "a" [], Node "b" []]
                      },
                treeCurrent = "new",
                treeBelow = makeCForest []
              }
       in treeCursorAddChildNodeAtStartAndSelect "new" [] tc `shouldBe` tc'
  describe "treeCursorAddChildNodeAtEndAndSelect" $ do
    it "produces valid cursors" $ producesValid3 (treeCursorAddChildNodeAtEndAndSelect @Bool)
    it "works for this example" $
      let tc =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = map makeCTree [Node "1" [], Node "2" []],
                        treeAboveAbove = Nothing,
                        treeAboveNode = "bar",
                        treeAboveRights = map makeCTree [Node "3" [], Node "4" []]
                      },
                treeCurrent = "baz",
                treeBelow = makeCForest [Node "a" [], Node "b" []]
              }
          tc' =
            TreeCursor
              { treeAbove =
                  Just
                    TreeAbove
                      { treeAboveLefts = map makeCTree [Node "b" [], Node "a" []],
                        treeAboveAbove =
                          Just
                            TreeAbove
                              { treeAboveLefts = map makeCTree [Node "1" [], Node "2" []],
                                treeAboveAbove = Nothing,
                                treeAboveNode = "bar",
                                treeAboveRights = map makeCTree [Node "3" [], Node "4" []]
                              },
                        treeAboveNode = "baz",
                        treeAboveRights = []
                      },
                treeCurrent = "new",
                treeBelow = makeCForest []
              }
       in treeCursorAddChildNodeAtEndAndSelect "new" [] tc `shouldBe` tc'
