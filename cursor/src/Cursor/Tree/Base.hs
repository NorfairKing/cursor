{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Tree.Base
  ( singletonTreeCursor
  , makeTreeCursor
  , makeTreeCursorWithSelection
  , rebuildTreeCursor
  , mapTreeCursor
  , currentTree
  , makeTreeCursorWithAbove
  , traverseTreeCursor
  , traverseTreeCursorSeq
  , foldTreeCursor
  , foldTreeCursorSeq
  ) where

import Control.Monad
import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence (Seq(..), (><), (|>))

import Cursor.Tree.Types

singletonTreeCursor :: a -> TreeCursor a b
singletonTreeCursor v = TreeCursor {treeAbove = Nothing, treeCurrent = v, treeBelow = emptyCForest}

makeTreeCursor :: (b -> a) -> CTree b -> TreeCursor a b
makeTreeCursor g (CNode v fs) = TreeCursor {treeAbove = Nothing, treeCurrent = g v, treeBelow = fs}

makeTreeCursorWithSelection ::
     (a -> b) -> (b -> a) -> TreeCursorSelection -> CTree b -> Maybe (TreeCursor a b)
makeTreeCursorWithSelection f g sel = walkDown sel . makeTreeCursor g
  where
    walkDown SelectNode tc = pure tc
    walkDown (SelectChild i s) TreeCursor {..} =
      (walkDown s =<<) $
      case splitAt i $ unpackCForest treeBelow of
        (_, []) -> Nothing
        (lefts, current:rights) ->
          Just $
          makeTreeCursorWithAbove g current $
          Just $
          TreeAbove
            { treeAboveLefts = S.fromList lefts
            , treeAboveAbove = treeAbove
            , treeAboveNode = f treeCurrent
            , treeAboveRights = S.fromList rights
            }

rebuildTreeCursor :: (a -> b) -> TreeCursor a b -> CTree b
rebuildTreeCursor f TreeCursor {..} = wrapAbove treeAbove $ CNode (f treeCurrent) treeBelow
  where
    wrapAbove Nothing t = t
    wrapAbove (Just TreeAbove {..}) t =
      wrapAbove treeAboveAbove $
      CNode treeAboveNode $ openForest $ toList $ (treeAboveLefts |> t) >< treeAboveRights

mapTreeCursor :: (a -> c) -> (b -> d) -> TreeCursor a b -> TreeCursor c d
mapTreeCursor f g TreeCursor {..} =
  TreeCursor
    {treeAbove = fmap g <$> treeAbove, treeCurrent = f treeCurrent, treeBelow = fmap g treeBelow}

currentTree :: (a -> b) -> TreeCursor a b -> CTree b
currentTree f TreeCursor {..} = CNode (f treeCurrent) treeBelow

makeTreeCursorWithAbove :: (b -> a) -> CTree b -> Maybe (TreeAbove b) -> TreeCursor a b
makeTreeCursorWithAbove g (CNode a forest) mta =
  TreeCursor {treeAbove = mta, treeCurrent = g a, treeBelow = forest}

traverseTreeCursor ::
     forall a b m c. Monad m
  => ([CTree b] -> b -> [CTree b] -> c -> m c)
  -> (a -> CForest b -> m c)
  -> TreeCursor a b
  -> m c
traverseTreeCursor wf =
  traverseTreeCursorSeq $ \befores cur afters -> wf (toList befores) cur (toList afters)

traverseTreeCursorSeq ::
     forall a b m c. Monad m
  => (Seq (CTree b) -> b -> Seq (CTree b) -> c -> m c)
  -> (a -> CForest b -> m c)
  -> TreeCursor a b
  -> m c
traverseTreeCursorSeq wrapFunc currentFunc TreeCursor {..} =
  currentFunc treeCurrent treeBelow >>= wrapAbove treeAbove
  where
    wrapAbove :: Maybe (TreeAbove b) -> c -> m c
    wrapAbove Nothing = pure
    wrapAbove (Just ta) = goAbove ta
    goAbove :: TreeAbove b -> c -> m c
    goAbove TreeAbove {..} =
      wrapFunc treeAboveLefts treeAboveNode treeAboveRights >=> wrapAbove treeAboveAbove

foldTreeCursor ::
     forall a b c.
     ([CTree b] -> b -> [CTree b] -> c -> c)
  -> (a -> CForest b -> c)
  -> TreeCursor a b
  -> c
foldTreeCursor wf =
  foldTreeCursorSeq $ \befores cur afters -> wf (toList befores) cur (toList afters)

foldTreeCursorSeq ::
     forall a b c.
     (Seq (CTree b) -> b -> Seq (CTree b) -> c -> c)
  -> (a -> CForest b -> c)
  -> TreeCursor a b
  -> c
foldTreeCursorSeq wrapFunc currentFunc TreeCursor {..} =
  wrapAbove treeAbove $ currentFunc treeCurrent treeBelow
  where
    wrapAbove :: Maybe (TreeAbove b) -> c -> c
    wrapAbove Nothing = id
    wrapAbove (Just ta) = goAbove ta
    goAbove :: TreeAbove b -> c -> c
    goAbove TreeAbove {..} =
      wrapAbove treeAboveAbove . wrapFunc treeAboveLefts treeAboveNode treeAboveRights
