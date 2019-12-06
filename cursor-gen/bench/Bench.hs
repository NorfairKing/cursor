{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion

import Data.GenValidity.Criterion

import Cursor.Forest.Gen ()
import Cursor.List.Gen ()
import Cursor.List.NonEmpty.Gen ()
import Cursor.Map.Gen ()
import Cursor.Map.KeyValue.Gen ()
import Cursor.Text.Gen ()
import Cursor.TextField.Gen ()
import Cursor.Tree.Gen ()

import Cursor.List
import Cursor.Simple.Forest
import Cursor.Simple.List.NonEmpty
import Cursor.Simple.Map
import Cursor.Simple.Tree
import Cursor.Text
import Cursor.TextField

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @(ListCursor Bool)
    , genValidBench @(NonEmptyCursor Bool)
    , genValidBench @(KeyValueCursor Bool Bool)
    , genValidBench @(MapCursor Bool Bool)
    , genValidBench @(TreeCursor Bool)
    , genValidBench @(ForestCursor Bool)
    , genValidBench @TextCursor
    , genValidBench @TextFieldCursor
    ]
